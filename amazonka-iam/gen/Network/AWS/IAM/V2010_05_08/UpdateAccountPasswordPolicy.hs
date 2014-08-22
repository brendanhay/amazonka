{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the password policy settings for the account. For more information
-- about using a password policy, see Managing an IAM Password Policy in the
-- Using IAM guide. https://iam.amazonaws.com/
-- ?Action=UpdateAccountPasswordPolicy &MinimumPasswordLength=12
-- &RequireSymbols=false &RequireNumbers=true &RequireUppercaseCharacters=true
-- &RequireLowercaseCharacters=true &AllowUsersToChangePassword=true
-- &MaxPasswordAge=90 &PasswordReusePrevention=6 &HardExpiry=false
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateAccountPasswordPolicy' request.
updateAccountPasswordPolicy :: UpdateAccountPasswordPolicy
updateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uapprHardExpiry = Nothing
    , _uapprRequireNumbers = Nothing
    , _uapprRequireLowercaseCharacters = Nothing
    , _uapprRequireSymbols = Nothing
    , _uapprRequireUppercaseCharacters = Nothing
    , _uapprAllowUsersToChangePassword = Nothing
    , _uapprMaxPasswordAge = Nothing
    , _uapprMinimumPasswordLength = Nothing
    , _uapprPasswordReusePrevention = Nothing
    }

data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uapprHardExpiry :: Maybe Bool
      -- ^ Prevents IAM users from setting a new password after their
      -- password has expired.
    , _uapprRequireNumbers :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one
      -- numeric character (0 to 9).
    , _uapprRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one
      -- lowercase character from the ISO basic Latin alphabet (a to z).
    , _uapprRequireSymbols :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one of
      -- the following non-alphanumeric characters: ! @ # $ % ^ &amp; * (
      -- ) _ + - = [ ] { } | '.
    , _uapprRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one
      -- uppercase character from the ISO basic Latin alphabet (A to Z).
    , _uapprAllowUsersToChangePassword :: Maybe Bool
      -- ^ Allows all IAM users in your account to use the AWS Management
      -- Console to change their own passwords. For more information, see
      -- Letting IAM Users Change Their Own Passwords in the Using IAM
      -- guide.
    , _uapprMaxPasswordAge :: Maybe Integer
      -- ^ The number of days that an IAM user password is valid.
    , _uapprMinimumPasswordLength :: Maybe Integer
      -- ^ The minimum number of characters allowed in an IAM user password.
    , _uapprPasswordReusePrevention :: Maybe Integer
      -- ^ Specifies the number of previous passwords that IAM users are
      -- prevented from reusing.
    } deriving (Show, Generic)

makeLenses ''UpdateAccountPasswordPolicy

instance ToQuery UpdateAccountPasswordPolicy where
    toQuery = genericQuery def

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateAccountPasswordPolicyResponse

instance AWSRequest UpdateAccountPasswordPolicy where
    type Sv UpdateAccountPasswordPolicy = IAM
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse

    request = post "UpdateAccountPasswordPolicy"
    response _ = nullaryResponse UpdateAccountPasswordPolicyResponse
