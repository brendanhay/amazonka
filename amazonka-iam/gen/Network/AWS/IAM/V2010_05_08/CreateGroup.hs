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
    , createGroup
    -- ** Request lenses
    , cgrGroupName
    , cgrPath

    -- * Response
    , CreateGroupResponse
    -- ** Response lenses
    , cgsGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateGroup' request.
createGroup :: Text -- ^ 'cgrGroupName'
            -> CreateGroup
createGroup p1 = CreateGroup
    { _cgrGroupName = p1
    , _cgrPath = Nothing
    }

data CreateGroup = CreateGroup
    { _cgrGroupName :: Text
      -- ^ Name of the group to create. Do not include the path in this
      -- value.
    , _cgrPath :: Maybe Text
      -- ^ The path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide. This
      -- parameter is optional. If it is not included, it defaults to a
      -- slash (/).
    } deriving (Show, Generic)

-- | Name of the group to create. Do not include the path in this value.
cgrGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateGroup
    -> f CreateGroup
cgrGroupName f x =
    (\y -> x { _cgrGroupName = y })
       <$> f (_cgrGroupName x)
{-# INLINE cgrGroupName #-}

-- | The path to the group. For more information about paths, see Identifiers
-- for IAM Entities in the Using IAM guide. This parameter is optional. If it
-- is not included, it defaults to a slash (/).
cgrPath
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateGroup
    -> f CreateGroup
cgrPath f x =
    (\y -> x { _cgrPath = y })
       <$> f (_cgrPath x)
{-# INLINE cgrPath #-}

instance ToQuery CreateGroup where
    toQuery = genericQuery def

data CreateGroupResponse = CreateGroupResponse
    { _cgsGroup :: Group
      -- ^ Information about the group.
    } deriving (Show, Generic)

-- | Information about the group.
cgsGroup
    :: Functor f
    => (Group
    -> f (Group))
    -> CreateGroupResponse
    -> f CreateGroupResponse
cgsGroup f x =
    (\y -> x { _cgsGroup = y })
       <$> f (_cgsGroup x)
{-# INLINE cgsGroup #-}

instance FromXML CreateGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateGroup where
    type Sv CreateGroup = IAM
    type Rs CreateGroup = CreateGroupResponse

    request = post "CreateGroup"
    response _ = xmlResponse
