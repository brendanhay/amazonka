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
    , createRole
    -- ** Request lenses
    , crrAssumeRolePolicyDocument
    , crrRoleName
    , crrPath

    -- * Response
    , CreateRoleResponse
    -- ** Response lenses
    , crsRole
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateRole' request.
createRole :: Text -- ^ 'crrAssumeRolePolicyDocument'
           -> Text -- ^ 'crrRoleName'
           -> CreateRole
createRole p1 p2 = CreateRole
    { _crrAssumeRolePolicyDocument = p1
    , _crrRoleName = p2
    , _crrPath = Nothing
    }
{-# INLINE createRole #-}

data CreateRole = CreateRole
    { _crrAssumeRolePolicyDocument :: Text
      -- ^ The policy that grants an entity permission to assume the role.
    , _crrRoleName :: Text
      -- ^ Name of the role to create.
    , _crrPath :: Maybe Text
      -- ^ The path to the role. For more information about paths, see
      -- Identifiers for IAM Entities in the Using IAM guide. This
      -- parameter is optional. If it is not included, it defaults to a
      -- slash (/).
    } deriving (Show, Generic)

-- | The policy that grants an entity permission to assume the role.
crrAssumeRolePolicyDocument :: Lens' CreateRole (Text)
crrAssumeRolePolicyDocument f x =
    f (_crrAssumeRolePolicyDocument x)
        <&> \y -> x { _crrAssumeRolePolicyDocument = y }
{-# INLINE crrAssumeRolePolicyDocument #-}

-- | Name of the role to create.
crrRoleName :: Lens' CreateRole (Text)
crrRoleName f x =
    f (_crrRoleName x)
        <&> \y -> x { _crrRoleName = y }
{-# INLINE crrRoleName #-}

-- | The path to the role. For more information about paths, see Identifiers for
-- IAM Entities in the Using IAM guide. This parameter is optional. If it is
-- not included, it defaults to a slash (/).
crrPath :: Lens' CreateRole (Maybe Text)
crrPath f x =
    f (_crrPath x)
        <&> \y -> x { _crrPath = y }
{-# INLINE crrPath #-}

instance ToQuery CreateRole where
    toQuery = genericQuery def

data CreateRoleResponse = CreateRoleResponse
    { _crsRole :: Role
      -- ^ Information about the role.
    } deriving (Show, Generic)

-- | Information about the role.
crsRole :: Lens' CreateRoleResponse (Role)
crsRole f x =
    f (_crsRole x)
        <&> \y -> x { _crsRole = y }
{-# INLINE crsRole #-}

instance FromXML CreateRoleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateRole where
    type Sv CreateRole = IAM
    type Rs CreateRole = CreateRoleResponse

    request = post "CreateRole"
    response _ = xmlResponse
