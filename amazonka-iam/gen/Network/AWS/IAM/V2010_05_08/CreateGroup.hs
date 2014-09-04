{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateGroup
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
module Network.AWS.IAM.V2010_05_08.CreateGroup
    (
    -- * Request
      CreateGroup
    -- ** Request constructor
    , mkCreateGroupRequest
    -- ** Request lenses
    , cgrPath
    , cgrGroupName

    -- * Response
    , CreateGroupResponse
    -- ** Response lenses
    , cgsGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateGroup' request.
mkCreateGroupRequest :: Text -- ^ 'cgrGroupName'
                     -> CreateGroup
mkCreateGroupRequest p1 = CreateGroup
    { _cgrPath = Nothing
    , _cgrGroupName = p2
    }
{-# INLINE mkCreateGroupRequest #-}

data CreateGroup = CreateGroup
    { _cgrPath :: Maybe Text
      -- ^ The path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide. This
      -- parameter is optional. If it is not included, it defaults to a
      -- slash (/).
    , _cgrGroupName :: Text
      -- ^ Name of the group to create. Do not include the path in this
      -- value.
    } deriving (Show, Generic)

-- | The path to the group. For more information about paths, see Identifiers
-- for IAM Entities in the Using IAM guide. This parameter is optional. If it
-- is not included, it defaults to a slash (/).
cgrPath :: Lens' CreateGroup (Maybe Text)
cgrPath = lens _cgrPath (\s a -> s { _cgrPath = a })
{-# INLINE cgrPath #-}

-- | Name of the group to create. Do not include the path in this value.
cgrGroupName :: Lens' CreateGroup (Text)
cgrGroupName = lens _cgrGroupName (\s a -> s { _cgrGroupName = a })
{-# INLINE cgrGroupName #-}

instance ToQuery CreateGroup where
    toQuery = genericQuery def

newtype CreateGroupResponse = CreateGroupResponse
    { _cgsGroup :: Group
      -- ^ Information about the group.
    } deriving (Show, Generic)

-- | Information about the group.
cgsGroup :: Lens' CreateGroupResponse (Group)
cgsGroup = lens _cgsGroup (\s a -> s { _cgsGroup = a })
{-# INLINE cgsGroup #-}

instance FromXML CreateGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateGroup where
    type Sv CreateGroup = IAM
    type Rs CreateGroup = CreateGroupResponse

    request = post "CreateGroup"
    response _ = xmlResponse
