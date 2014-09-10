{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRole &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-08T23:34:01Z AROADBQP57FF2AEXAMPLE
-- df37e965-9967-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM
    (
    -- * Request
      GetRole
    -- ** Request constructor
    , mkGetRole
    -- ** Request lenses
    , grRoleName

    -- * Response
    , GetRoleResponse
    -- ** Response constructor
    , mkGetRoleResponse
    -- ** Response lenses
    , grrRole
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype GetRole = GetRole
    { _grRoleName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetRole' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
mkGetRole :: Text -- ^ 'grRoleName'
          -> GetRole
mkGetRole p1 = GetRole
    { _grRoleName = p1
    }

-- | Name of the role to get information about.
grRoleName :: Lens' GetRole Text
grRoleName = lens _grRoleName (\s a -> s { _grRoleName = a })

instance ToQuery GetRole where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetRole action.
newtype GetRoleResponse = GetRoleResponse
    { _grrRole :: Role
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetRoleResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Role ::@ @Role@
--
mkGetRoleResponse :: Role -- ^ 'grrRole'
                  -> GetRoleResponse
mkGetRoleResponse p1 = GetRoleResponse
    { _grrRole = p1
    }

-- | Information about the role.
grrRole :: Lens' GetRoleResponse Role
grrRole = lens _grrRole (\s a -> s { _grrRole = a })

instance FromXML GetRoleResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetRole where
    type Sv GetRole = IAM
    type Rs GetRole = GetRoleResponse

    request = post "GetRole"
    response _ = xmlResponse
