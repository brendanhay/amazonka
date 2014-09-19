{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
-- https://iam.amazonaws.com/ ?Action=GetAccountPasswordPolicy
-- &Version=2010-05-08 &AUTHPARAMS true true true false 12 true 90 false true
-- 12 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetAccountPasswordPolicy
    (
    -- * Request
      GetAccountPasswordPolicy
    -- ** Request constructor
    , getAccountPasswordPolicy
    -- * Response
    , GetAccountPasswordPolicyResponse
    -- ** Response constructor
    , getAccountPasswordPolicyResponse
    -- ** Response lenses
    , gapprPasswordPolicy
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetAccountPasswordPolicy = GetAccountPasswordPolicy
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetAccountPasswordPolicy' request.
getAccountPasswordPolicy :: GetAccountPasswordPolicy
getAccountPasswordPolicy = GetAccountPasswordPolicy

instance ToQuery GetAccountPasswordPolicy where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- GetAccountPasswordPolicy action.
newtype GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse
    { _gapprPasswordPolicy :: PasswordPolicy
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetAccountPasswordPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PasswordPolicy ::@ @PasswordPolicy@
--
getAccountPasswordPolicyResponse :: PasswordPolicy -- ^ 'gapprPasswordPolicy'
                                   -> GetAccountPasswordPolicyResponse
getAccountPasswordPolicyResponse p1 = GetAccountPasswordPolicyResponse
    { _gapprPasswordPolicy = p1
    }

-- | The PasswordPolicy data type contains information about the account
-- password policy. This data type is used as a response element in the action
-- GetAccountPasswordPolicy.
gapprPasswordPolicy :: Lens' GetAccountPasswordPolicyResponse PasswordPolicy
gapprPasswordPolicy =
    lens _gapprPasswordPolicy (\s a -> s { _gapprPasswordPolicy = a })

instance FromXML GetAccountPasswordPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetAccountPasswordPolicy where
    type Sv GetAccountPasswordPolicy = IAM
    type Rs GetAccountPasswordPolicy = GetAccountPasswordPolicyResponse

    request = post "GetAccountPasswordPolicy"
    response _ = xmlResponse
