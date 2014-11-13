{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified role, including the role's path,
-- GUID, ARN, and the policy granting permission to assume the role. For more
-- information about ARNs, go to ARNs. For more information about roles, go to
-- Working with Roles. The returned policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html.
module Network.AWS.IAM.GetRole
    (
    -- * Request
      GetRole
    -- ** Request constructor
    , getRole
    -- ** Request lenses
    , grRoleName

    -- * Response
    , GetRoleResponse
    -- ** Response constructor
    , getRoleResponse
    -- ** Response lenses
    , grrRole
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetRole = GetRole
    { _grRoleName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grRoleName' @::@ 'Text'
--
getRole :: Text -- ^ 'grRoleName'
        -> GetRole
getRole p1 = GetRole
    { _grRoleName = p1
    }

-- | The name of the role to get information about.
grRoleName :: Lens' GetRole Text
grRoleName = lens _grRoleName (\s a -> s { _grRoleName = a })

instance ToQuery GetRole

instance ToPath GetRole where
    toPath = const "/"

newtype GetRoleResponse = GetRoleResponse
    { _grrRole :: Role
    } deriving (Eq, Show, Generic)

-- | 'GetRoleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrRole' @::@ 'Role'
--
getRoleResponse :: Role -- ^ 'grrRole'
                -> GetRoleResponse
getRoleResponse p1 = GetRoleResponse
    { _grrRole = p1
    }

-- | Information about the role.
grrRole :: Lens' GetRoleResponse Role
grrRole = lens _grrRole (\s a -> s { _grrRole = a })

instance AWSRequest GetRole where
    type Sv GetRole = IAM
    type Rs GetRole = GetRoleResponse

    request  = post "GetRole"
    response = xmlResponse $ \h x -> GetRoleResponse
        <$> x %| "Role"
