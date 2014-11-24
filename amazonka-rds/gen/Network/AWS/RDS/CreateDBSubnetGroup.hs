{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB subnet group. DB subnet groups must contain at least one
-- subnet in at least two AZs in the region.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSubnetGroup.html>
module Network.AWS.RDS.CreateDBSubnetGroup
    (
    -- * Request
      CreateDBSubnetGroup
    -- ** Request constructor
    , createDBSubnetGroup
    -- ** Request lenses
    , cdbsg1DBSubnetGroupDescription
    , cdbsg1DBSubnetGroupName
    , cdbsg1SubnetIds
    , cdbsg1Tags

    -- * Response
    , CreateDBSubnetGroupResponse
    -- ** Response constructor
    , createDBSubnetGroupResponse
    -- ** Response lenses
    , cdbsgrDBSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CreateDBSubnetGroup = CreateDBSubnetGroup
    { _cdbsg1DBSubnetGroupDescription :: Text
    , _cdbsg1DBSubnetGroupName        :: Text
    , _cdbsg1SubnetIds                :: List "SubnetIdentifier" Text
    , _cdbsg1Tags                     :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateDBSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsg1DBSubnetGroupDescription' @::@ 'Text'
--
-- * 'cdbsg1DBSubnetGroupName' @::@ 'Text'
--
-- * 'cdbsg1SubnetIds' @::@ ['Text']
--
-- * 'cdbsg1Tags' @::@ ['Tag']
--
createDBSubnetGroup :: Text -- ^ 'cdbsg1DBSubnetGroupName'
                    -> Text -- ^ 'cdbsg1DBSubnetGroupDescription'
                    -> CreateDBSubnetGroup
createDBSubnetGroup p1 p2 = CreateDBSubnetGroup
    { _cdbsg1DBSubnetGroupName        = p1
    , _cdbsg1DBSubnetGroupDescription = p2
    , _cdbsg1SubnetIds                = mempty
    , _cdbsg1Tags                     = mempty
    }

-- | The description for the DB subnet group.
cdbsg1DBSubnetGroupDescription :: Lens' CreateDBSubnetGroup Text
cdbsg1DBSubnetGroupDescription =
    lens _cdbsg1DBSubnetGroupDescription
        (\s a -> s { _cdbsg1DBSubnetGroupDescription = a })

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Example: @mySubnetgroup@.
cdbsg1DBSubnetGroupName :: Lens' CreateDBSubnetGroup Text
cdbsg1DBSubnetGroupName =
    lens _cdbsg1DBSubnetGroupName (\s a -> s { _cdbsg1DBSubnetGroupName = a })

-- | The EC2 Subnet IDs for the DB subnet group.
cdbsg1SubnetIds :: Lens' CreateDBSubnetGroup [Text]
cdbsg1SubnetIds = lens _cdbsg1SubnetIds (\s a -> s { _cdbsg1SubnetIds = a }) . _List

cdbsg1Tags :: Lens' CreateDBSubnetGroup [Tag]
cdbsg1Tags = lens _cdbsg1Tags (\s a -> s { _cdbsg1Tags = a }) . _List

newtype CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse
    { _cdbsgrDBSubnetGroup :: Maybe DBSubnetGroup
    } deriving (Eq, Show)

-- | 'CreateDBSubnetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgrDBSubnetGroup' @::@ 'Maybe' 'DBSubnetGroup'
--
createDBSubnetGroupResponse :: CreateDBSubnetGroupResponse
createDBSubnetGroupResponse = CreateDBSubnetGroupResponse
    { _cdbsgrDBSubnetGroup = Nothing
    }

cdbsgrDBSubnetGroup :: Lens' CreateDBSubnetGroupResponse (Maybe DBSubnetGroup)
cdbsgrDBSubnetGroup =
    lens _cdbsgrDBSubnetGroup (\s a -> s { _cdbsgrDBSubnetGroup = a })

instance ToPath CreateDBSubnetGroup where
    toPath = const "/"

instance ToQuery CreateDBSubnetGroup where
    toQuery CreateDBSubnetGroup{..} = mconcat
        [ "DBSubnetGroupDescription" =? _cdbsg1DBSubnetGroupDescription
        , "DBSubnetGroupName"        =? _cdbsg1DBSubnetGroupName
        , "SubnetIds"                =? _cdbsg1SubnetIds
        , "Tags"                     =? _cdbsg1Tags
        ]

instance ToHeaders CreateDBSubnetGroup

instance AWSRequest CreateDBSubnetGroup where
    type Sv CreateDBSubnetGroup = RDS
    type Rs CreateDBSubnetGroup = CreateDBSubnetGroupResponse

    request  = post "CreateDBSubnetGroup"
    response = xmlResponse

instance FromXML CreateDBSubnetGroupResponse where
    parseXML = withElement "CreateDBSubnetGroupResult" $ \x -> CreateDBSubnetGroupResponse
        <$> x .@? "DBSubnetGroup"
