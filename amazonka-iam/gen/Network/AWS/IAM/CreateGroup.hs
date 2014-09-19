{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new group. For information about the number of groups you can
-- create, see Limitations on IAM Entities in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=CreateGroup &Path=/ &GroupName=Admins
-- &Version=2010-05-08 &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateGroup
    (
    -- * Request
      CreateGroup
    -- ** Request constructor
    , createGroup
    -- ** Request lenses
    , cgPath
    , cgGroupName

    -- * Response
    , CreateGroupResponse
    -- ** Response constructor
    , createGroupResponse
    -- ** Response lenses
    , cgrGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data CreateGroup = CreateGroup
    { _cgPath :: Maybe Text
    , _cgGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Path ::@ @Maybe Text@
--
-- * @GroupName ::@ @Text@
--
createGroup :: Text -- ^ 'cgGroupName'
            -> CreateGroup
createGroup p2 = CreateGroup
    { _cgPath = Nothing
    , _cgGroupName = p2
    }

-- | The path to the group. For more information about paths, see Identifiers
-- for IAM Entities in the Using IAM guide. This parameter is optional. If it
-- is not included, it defaults to a slash (/).
cgPath :: Lens' CreateGroup (Maybe Text)
cgPath = lens _cgPath (\s a -> s { _cgPath = a })

-- | Name of the group to create. Do not include the path in this value.
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\s a -> s { _cgGroupName = a })

instance ToQuery CreateGroup where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the CreateGroup action.
newtype CreateGroupResponse = CreateGroupResponse
    { _cgrGroup :: Group
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Group ::@ @Group@
--
createGroupResponse :: Group -- ^ 'cgrGroup'
                    -> CreateGroupResponse
createGroupResponse p1 = CreateGroupResponse
    { _cgrGroup = p1
    }

-- | Information about the group.
cgrGroup :: Lens' CreateGroupResponse Group
cgrGroup = lens _cgrGroup (\s a -> s { _cgrGroup = a })

instance FromXML CreateGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateGroup where
    type Sv CreateGroup = IAM
    type Rs CreateGroup = CreateGroupResponse

    request = post "CreateGroup"
    response _ = xmlResponse
