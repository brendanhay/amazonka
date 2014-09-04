{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy
    (
    -- * Request
      UpdateAccountPasswordPolicy
    -- ** Request constructor
    , mkUpdateAccountPasswordPolicyRequest
    -- ** Request lenses
    , uapprMinimumPasswordLength
    , uapprRequireSymbols
    , uapprRequireNumbers
    , uapprRequireUppercaseCharacters
    , uapprRequireLowercaseCharacters
    , uapprAllowUsersToChangePassword
    , uapprMaxPasswordAge
    , uapprPasswordReusePrevention
    , uapprHardExpiry

    -- * Response
    , UpdateAccountPasswordPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAccountPasswordPolicy' request.
mkUpdateAccountPasswordPolicyRequest :: UpdateAccountPasswordPolicy
mkUpdateAccountPasswordPolicyRequest = UpdateAccountPasswordPolicy
    { _uapprMinimumPasswordLength = Nothing
    , _uapprRequireSymbols = Nothing
    , _uapprRequireNumbers = Nothing
    , _uapprRequireUppercaseCharacters = Nothing
    , _uapprRequireLowercaseCharacters = Nothing
    , _uapprAllowUsersToChangePassword = Nothing
    , _uapprMaxPasswordAge = Nothing
    , _uapprPasswordReusePrevention = Nothing
    , _uapprHardExpiry = Nothing
    }
{-# INLINE mkUpdateAccountPasswordPolicyRequest #-}

data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uapprMinimumPasswordLength :: Maybe Integer
      -- ^ The minimum number of characters allowed in an IAM user password.
    , _uapprRequireSymbols :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one of
      -- the following non-alphanumeric characters: ! @ # $ % ^ &amp; * (
      -- ) _ + - = [ ] { } | '.
    , _uapprRequireNumbers :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one
      -- numeric character (0 to 9).
    , _uapprRequireUppercaseCharacters :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one
      -- uppercase character from the ISO basic Latin alphabet (A to Z).
    , _uapprRequireLowercaseCharacters :: Maybe Bool
      -- ^ Specifies whether IAM user passwords must contain at least one
      -- lowercase character from the ISO basic Latin alphabet (a to z).
    , _uapprAllowUsersToChangePassword :: Maybe Bool
      -- ^ Allows all IAM users in your account to use the AWS Management
      -- Console to change their own passwords. For more information, see
      -- Letting IAM Users Change Their Own Passwords in the Using IAM
      -- guide.
    , _uapprMaxPasswordAge :: Maybe Integer
      -- ^ The number of days that an IAM user password is valid.
    , _uapprPasswordReusePrevention :: Maybe Integer
      -- ^ Specifies the number of previous passwords that IAM users are
      -- prevented from reusing.
    , _uapprHardExpiry :: Maybe Bool
      -- ^ Prevents IAM users from setting a new password after their
      -- password has expired.
    } deriving (Show, Generic)

-- | The minimum number of characters allowed in an IAM user password.
uapprMinimumPasswordLength :: Lens' UpdateAccountPasswordPolicy (Maybe Integer)
uapprMinimumPasswordLength = lens _uapprMinimumPasswordLength (\s a -> s { _uapprMinimumPasswordLength = a })
{-# INLINE uapprMinimumPasswordLength #-}

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters: ! @ # $ % ^ &amp; * ( ) _ + - = [ ]
-- { } | '.
uapprRequireSymbols :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprRequireSymbols = lens _uapprRequireSymbols (\s a -> s { _uapprRequireSymbols = a })
{-# INLINE uapprRequireSymbols #-}

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
uapprRequireNumbers :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprRequireNumbers = lens _uapprRequireNumbers (\s a -> s { _uapprRequireNumbers = a })
{-# INLINE uapprRequireNumbers #-}

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
uapprRequireUppercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprRequireUppercaseCharacters = lens _uapprRequireUppercaseCharacters (\s a -> s { _uapprRequireUppercaseCharacters = a })
{-# INLINE uapprRequireUppercaseCharacters #-}

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
uapprRequireLowercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprRequireLowercaseCharacters = lens _uapprRequireLowercaseCharacters (\s a -> s { _uapprRequireLowercaseCharacters = a })
{-# INLINE uapprRequireLowercaseCharacters #-}

-- | Allows all IAM users in your account to use the AWS Management Console to
-- change their own passwords. For more information, see Letting IAM Users
-- Change Their Own Passwords in the Using IAM guide.
uapprAllowUsersToChangePassword :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprAllowUsersToChangePassword = lens _uapprAllowUsersToChangePassword (\s a -> s { _uapprAllowUsersToChangePassword = a })
{-# INLINE uapprAllowUsersToChangePassword #-}

-- | The number of days that an IAM user password is valid.
uapprMaxPasswordAge :: Lens' UpdateAccountPasswordPolicy (Maybe Integer)
uapprMaxPasswordAge = lens _uapprMaxPasswordAge (\s a -> s { _uapprMaxPasswordAge = a })
{-# INLINE uapprMaxPasswordAge #-}

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
uapprPasswordReusePrevention :: Lens' UpdateAccountPasswordPolicy (Maybe Integer)
uapprPasswordReusePrevention = lens _uapprPasswordReusePrevention (\s a -> s { _uapprPasswordReusePrevention = a })
{-# INLINE uapprPasswordReusePrevention #-}

-- | Prevents IAM users from setting a new password after their password has
-- expired.
uapprHardExpiry :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprHardExpiry = lens _uapprHardExpiry (\s a -> s { _uapprHardExpiry = a })
{-# INLINE uapprHardExpiry #-}

instance ToQuery UpdateAccountPasswordPolicy where
    toQuery = genericQuery def

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateAccountPasswordPolicy where
    type Sv UpdateAccountPasswordPolicy = IAM
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse

    request = post "UpdateAccountPasswordPolicy"
    response _ = nullaryResponse UpdateAccountPasswordPolicyResponse
