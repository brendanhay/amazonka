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

-- Module      : Network.AWS.IAM.CreateRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new role for your AWS account. For more information about roles,
-- go to Working with Roles. For information about limitations on role names
-- and the number of roles you can create, go to Limitations on IAM Entities
-- in the Using IAM guide. The example policy grants permission to an EC2
-- instance to assume the role. The policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html.
module Network.AWS.IAM.CreateRole
    (
    -- * Request
      CreateRole
    -- ** Request constructor
    , createRole
    -- ** Request lenses
    , crAssumeRolePolicyDocument
    , crPath
    , crRoleName

    -- * Response
    , CreateRoleResponse
    -- ** Response constructor
    , createRoleResponse
    -- ** Response lenses
    , crrRole
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreateRole = CreateRole
    { _crAssumeRolePolicyDocument :: Text
    , _crPath                     :: Maybe Text
    , _crRoleName                 :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crAssumeRolePolicyDocument' @::@ 'Text'
--
-- * 'crPath' @::@ 'Maybe' 'Text'
--
-- * 'crRoleName' @::@ 'Text'
--
createRole :: Text -- ^ 'crRoleName'
           -> Text -- ^ 'crAssumeRolePolicyDocument'
           -> CreateRole
createRole p1 p2 = CreateRole
    { _crRoleName                 = p1
    , _crAssumeRolePolicyDocument = p2
    , _crPath                     = Nothing
    }

-- | The policy that grants an entity permission to assume the role.
crAssumeRolePolicyDocument :: Lens' CreateRole Text
crAssumeRolePolicyDocument =
    lens _crAssumeRolePolicyDocument
        (\s a -> s { _crAssumeRolePolicyDocument = a })

-- | The path to the role. For more information about paths, see IAM
-- Identifiers in the Using IAM guide. This parameter is optional. If it is
-- not included, it defaults to a slash (/).
crPath :: Lens' CreateRole (Maybe Text)
crPath = lens _crPath (\s a -> s { _crPath = a })

-- | The name of the role to create.
crRoleName :: Lens' CreateRole Text
crRoleName = lens _crRoleName (\s a -> s { _crRoleName = a })

instance ToQuery CreateRole

instance ToPath CreateRole where
    toPath = const "/"

newtype CreateRoleResponse = CreateRoleResponse
    { _crrRole :: Role
    } deriving (Eq, Show, Generic)

-- | 'CreateRoleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrRole' @::@ 'Role'
--
createRoleResponse :: Role -- ^ 'crrRole'
                   -> CreateRoleResponse
createRoleResponse p1 = CreateRoleResponse
    { _crrRole = p1
    }

-- | Information about the role.
crrRole :: Lens' CreateRoleResponse Role
crrRole = lens _crrRole (\s a -> s { _crrRole = a })

instance AWSRequest CreateRole where
    type Sv CreateRole = IAM
    type Rs CreateRole = CreateRoleResponse

    request  = post "CreateRole"
    response = xmlResponse $ \h x -> CreateRoleResponse
        <$> x %| "Role"
