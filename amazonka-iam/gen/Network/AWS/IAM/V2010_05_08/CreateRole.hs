{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateRole
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
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=CreateRole &RoleName=S3Access &Path=/application_abc/component_xyz/
-- &AssumeRolePolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &Version=2010-05-08 &AUTHPARAMS /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-08T23:34:01.495Z AROADBQP57FF2AEXAMPLE
-- 4a93ceee-9966-11e1-b624-b1aEXAMPLE7c.
module Network.AWS.IAM.V2010_05_08.CreateRole
    (
    -- * Request
      CreateRole
    -- ** Request constructor
    , mkCreateRole
    -- ** Request lenses
    , crPath
    , crRoleName
    , crAssumeRolePolicyDocument

    -- * Response
    , CreateRoleResponse
    -- ** Response lenses
    , crrsRole
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data CreateRole = CreateRole
    { _crPath :: Maybe Text
    , _crRoleName :: Text
    , _crAssumeRolePolicyDocument :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateRole' request.
mkCreateRole :: Text -- ^ 'crRoleName'
             -> Text -- ^ 'crAssumeRolePolicyDocument'
             -> CreateRole
mkCreateRole p2 p3 = CreateRole
    { _crPath = Nothing
    , _crRoleName = p2
    , _crAssumeRolePolicyDocument = p3
    }
{-# INLINE mkCreateRole #-}

-- | The path to the role. For more information about paths, see Identifiers for
-- IAM Entities in the Using IAM guide. This parameter is optional. If it is
-- not included, it defaults to a slash (/).
crPath :: Lens' CreateRole (Maybe Text)
crPath = lens _crPath (\s a -> s { _crPath = a })
{-# INLINE crPath #-}

-- | Name of the role to create.
crRoleName :: Lens' CreateRole Text
crRoleName = lens _crRoleName (\s a -> s { _crRoleName = a })
{-# INLINE crRoleName #-}

-- | The policy that grants an entity permission to assume the role.
crAssumeRolePolicyDocument :: Lens' CreateRole Text
crAssumeRolePolicyDocument =
    lens _crAssumeRolePolicyDocument
         (\s a -> s { _crAssumeRolePolicyDocument = a })
{-# INLINE crAssumeRolePolicyDocument #-}

instance ToQuery CreateRole where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the CreateRole action.
newtype CreateRoleResponse = CreateRoleResponse
    { _crrsRole :: Role
    } deriving (Show, Generic)

-- | Information about the role.
crrsRole :: Lens' CreateRoleResponse Role
crrsRole = lens _crrsRole (\s a -> s { _crrsRole = a })
{-# INLINE crrsRole #-}

instance FromXML CreateRoleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateRole where
    type Sv CreateRole = IAM
    type Rs CreateRole = CreateRoleResponse

    request = post "CreateRole"
    response _ = xmlResponse
