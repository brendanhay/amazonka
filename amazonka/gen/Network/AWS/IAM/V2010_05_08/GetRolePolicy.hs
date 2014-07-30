{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified role. For more
-- information about roles, go to Working with Roles. The returned policy is
-- URL-encoded according to RFC 3986. For more information about RFC 3986, go
-- to http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRolePolicy &PolicyName=S3AccessPolicy &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS S3AccessPolicy S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["s3:*"],"Resource":["*"]}]}
-- 7e7cd8bc-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.GetRolePolicy where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.IAM.V2010_05_08.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data GetRolePolicy = GetRolePolicy
    { _grprPolicyName :: Text
      -- ^ Name of the policy document to get.
    , _grprRoleName :: Text
      -- ^ Name of the role associated with the policy.
    } deriving (Generic)

instance ToQuery GetRolePolicy where
    toQuery = genericToQuery def

instance AWSRequest GetRolePolicy where
    type Sv GetRolePolicy = IAM
    type Rs GetRolePolicy = GetRolePolicyResponse

    request = post "GetRolePolicy"
    response _ = xmlResponse

data GetRolePolicyResponse = GetRolePolicyResponse
    { _grpsPolicyDocument :: Text
      -- ^ The policy document.
    , _grpsPolicyName :: Text
      -- ^ The name of the policy.
    , _grpsRoleName :: Text
      -- ^ The role the policy is associated with.
    } deriving (Generic)

instance FromXML GetRolePolicyResponse where
    fromXMLOptions = xmlOptions
