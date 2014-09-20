{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB parameter group. A DB parameter group is initially created
-- with the default parameters for the database engine used by the DB
-- instance. To provide custom values for any of the parameters, you must
-- modify the group after creating it using ModifyDBParameterGroup. Once
-- you've created a DB parameter group, you need to associate it with your DB
-- instance using ModifyDBInstance. When you associate a new DB parameter
-- group with a running DB instance, you need to reboot the DB Instance for
-- the new DB parameter group and associated settings to take effect.
-- https://rds.amazonaws.com/ ?Action=CreateDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup3 &DBParameterGroupFamily=MySQL5.1
-- &Version=2013-05-15 &Description=My%20new%20DBParameterGroup
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T18%3A09%3A29.793Z &AWSAccessKeyId= &Signature=
-- mysql5.1 My new DBParameterGroup mydbparametergroup3
-- 0b447b66-bf36-11de-a88b-7b5b3d23b3a7.
module Network.AWS.RDS.CreateDBParameterGroup
    (
    -- * Request
      CreateDBParameterGroup
    -- ** Request constructor
    , createDBParameterGroup
    -- ** Request lenses
    , cdbpgDBParameterGroupName
    , cdbpgDBParameterGroupFamily
    , cdbpgDescription
    , cdbpgTags

    -- * Response
    , CreateDBParameterGroupResponse
    -- ** Response constructor
    , createDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgrDBParameterGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CreateDBParameterGroup = CreateDBParameterGroup
    { _cdbpgDBParameterGroupName :: Text
    , _cdbpgDBParameterGroupFamily :: Text
    , _cdbpgDescription :: Text
    , _cdbpgTags :: [Tag]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Text@
--
-- * @DBParameterGroupFamily ::@ @Text@
--
-- * @Description ::@ @Text@
--
-- * @Tags ::@ @[Tag]@
--
createDBParameterGroup :: Text -- ^ 'cdbpgDBParameterGroupName'
                       -> Text -- ^ 'cdbpgDBParameterGroupFamily'
                       -> Text -- ^ 'cdbpgDescription'
                       -> CreateDBParameterGroup
createDBParameterGroup p1 p2 p3 = CreateDBParameterGroup
    { _cdbpgDBParameterGroupName = p1
    , _cdbpgDBParameterGroupFamily = p2
    , _cdbpgDescription = p3
    , _cdbpgTags = mempty
    }

-- | The name of the DB parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens This value is stored as a
-- lower-case string.
cdbpgDBParameterGroupName :: Lens' CreateDBParameterGroup Text
cdbpgDBParameterGroupName =
    lens _cdbpgDBParameterGroupName
         (\s a -> s { _cdbpgDBParameterGroupName = a })

-- | The DB parameter group family name. A DB parameter group can be associated
-- with one and only one DB parameter group family, and can be applied only to
-- a DB instance running a database engine and engine version compatible with
-- that DB parameter group family.
cdbpgDBParameterGroupFamily :: Lens' CreateDBParameterGroup Text
cdbpgDBParameterGroupFamily =
    lens _cdbpgDBParameterGroupFamily
         (\s a -> s { _cdbpgDBParameterGroupFamily = a })

-- | The description for the DB parameter group.
cdbpgDescription :: Lens' CreateDBParameterGroup Text
cdbpgDescription =
    lens _cdbpgDescription (\s a -> s { _cdbpgDescription = a })

-- | A list of tags.
cdbpgTags :: Lens' CreateDBParameterGroup [Tag]
cdbpgTags = lens _cdbpgTags (\s a -> s { _cdbpgTags = a })

instance ToQuery CreateDBParameterGroup where
    toQuery = genericQuery def

newtype CreateDBParameterGroupResponse = CreateDBParameterGroupResponse
    { _cdbpgrDBParameterGroup :: Maybe DBParameterGroup
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroup ::@ @Maybe DBParameterGroup@
--
createDBParameterGroupResponse :: CreateDBParameterGroupResponse
createDBParameterGroupResponse = CreateDBParameterGroupResponse
    { _cdbpgrDBParameterGroup = Nothing
    }

-- | Contains the result of a successful invocation of the
-- CreateDBParameterGroup action. This data type is used as a request
-- parameter in the DeleteDBParameterGroup action, and as a response element
-- in the DescribeDBParameterGroups action.
cdbpgrDBParameterGroup :: Lens' CreateDBParameterGroupResponse (Maybe DBParameterGroup)
cdbpgrDBParameterGroup =
    lens _cdbpgrDBParameterGroup (\s a -> s { _cdbpgrDBParameterGroup = a })

instance FromXML CreateDBParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBParameterGroup where
    type Sv CreateDBParameterGroup = RDS
    type Rs CreateDBParameterGroup = CreateDBParameterGroupResponse

    request = post "CreateDBParameterGroup"
    response _ = xmlResponse
