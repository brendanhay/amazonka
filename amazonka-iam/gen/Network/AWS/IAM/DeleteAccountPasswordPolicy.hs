{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the password policy for the AWS account. https://iam.amazonaws.com/
-- ?Action=DeleteAccountPasswordPolicy &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM
    (
    -- * Request
      DeleteAccountPasswordPolicy
    -- ** Request constructor
    , mkDeleteAccountPasswordPolicy
    -- * Response
    , DeleteAccountPasswordPolicyResponse
    -- ** Response constructor
    , mkDeleteAccountPasswordPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAccountPasswordPolicy' request.
mkDeleteAccountPasswordPolicy :: DeleteAccountPasswordPolicy
mkDeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy

instance ToQuery DeleteAccountPasswordPolicy where
    toQuery = genericQuery def

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAccountPasswordPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteAccountPasswordPolicyResponse :: DeleteAccountPasswordPolicyResponse
mkDeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse

instance AWSRequest DeleteAccountPasswordPolicy where
    type Sv DeleteAccountPasswordPolicy = IAM
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse

    request = post "DeleteAccountPasswordPolicy"
    response _ = nullaryResponse DeleteAccountPasswordPolicyResponse
