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
    , updateAccountPasswordPolicy
    -- ** Request lenses
    , uapprHardExpiry
    , uapprRequireSymbols
    , uapprRequireNumbers
    , uapprRequireUppercaseCharacters
    , uapprRequireLowercaseCharacters
    , uapprAllowUsersToChangePassword
    , uapprMaxPasswordAge
    , uapprMinimumPasswordLength
    , uapprPasswordReusePrevention

    -- * Response
    , UpdateAccountPasswordPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateAccountPasswordPolicy' request.
updateAccountPasswordPolicy :: UpdateAccountPasswordPolicy
updateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uapprHardExpiry = Nothing
    , _uapprRequireSymbols = Nothing
    , _uapprRequireNumbers = Nothing
    , _uapprRequireUppercaseCharacters = Nothing
    , _uapprRequireLowercaseCharacters = Nothing
    , _uapprAllowUsersToChangePassword = Nothing
    , _uapprMaxPasswordAge = Nothing
    , _uapprMinimumPasswordLength = Nothing
    , _uapprPasswordReusePrevention = Nothing
    }

data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uapprHardExpiry :: Maybe Bool
      -- ^ Prevents IAM users from setting a new password after their
      -- password has expired.
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
    , _uapprMinimumPasswordLength :: Maybe Integer
      -- ^ The minimum number of characters allowed in an IAM user password.
    , _uapprPasswordReusePrevention :: Maybe Integer
      -- ^ Specifies the number of previous passwords that IAM users are
      -- prevented from reusing.
    } deriving (Show, Generic)

-- | Prevents IAM users from setting a new password after their password has
-- expired.
uapprHardExpiry
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprHardExpiry f x =
    (\y -> x { _uapprHardExpiry = y })
       <$> f (_uapprHardExpiry x)
{-# INLINE uapprHardExpiry #-}

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters: ! @ # $ % ^ &amp; * ( ) _ + - = [ ]
-- { } | '.
uapprRequireSymbols
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprRequireSymbols f x =
    (\y -> x { _uapprRequireSymbols = y })
       <$> f (_uapprRequireSymbols x)
{-# INLINE uapprRequireSymbols #-}

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
uapprRequireNumbers
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprRequireNumbers f x =
    (\y -> x { _uapprRequireNumbers = y })
       <$> f (_uapprRequireNumbers x)
{-# INLINE uapprRequireNumbers #-}

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
uapprRequireUppercaseCharacters
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprRequireUppercaseCharacters f x =
    (\y -> x { _uapprRequireUppercaseCharacters = y })
       <$> f (_uapprRequireUppercaseCharacters x)
{-# INLINE uapprRequireUppercaseCharacters #-}

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
uapprRequireLowercaseCharacters
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprRequireLowercaseCharacters f x =
    (\y -> x { _uapprRequireLowercaseCharacters = y })
       <$> f (_uapprRequireLowercaseCharacters x)
{-# INLINE uapprRequireLowercaseCharacters #-}

-- | Allows all IAM users in your account to use the AWS Management Console to
-- change their own passwords. For more information, see Letting IAM Users
-- Change Their Own Passwords in the Using IAM guide.
uapprAllowUsersToChangePassword
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprAllowUsersToChangePassword f x =
    (\y -> x { _uapprAllowUsersToChangePassword = y })
       <$> f (_uapprAllowUsersToChangePassword x)
{-# INLINE uapprAllowUsersToChangePassword #-}

-- | The number of days that an IAM user password is valid.
uapprMaxPasswordAge
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprMaxPasswordAge f x =
    (\y -> x { _uapprMaxPasswordAge = y })
       <$> f (_uapprMaxPasswordAge x)
{-# INLINE uapprMaxPasswordAge #-}

-- | The minimum number of characters allowed in an IAM user password.
uapprMinimumPasswordLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprMinimumPasswordLength f x =
    (\y -> x { _uapprMinimumPasswordLength = y })
       <$> f (_uapprMinimumPasswordLength x)
{-# INLINE uapprMinimumPasswordLength #-}

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
uapprPasswordReusePrevention
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UpdateAccountPasswordPolicy
    -> f UpdateAccountPasswordPolicy
uapprPasswordReusePrevention f x =
    (\y -> x { _uapprPasswordReusePrevention = y })
       <$> f (_uapprPasswordReusePrevention x)
{-# INLINE uapprPasswordReusePrevention #-}

instance ToQuery UpdateAccountPasswordPolicy where
    toQuery = genericQuery def

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateAccountPasswordPolicy where
    type Sv UpdateAccountPasswordPolicy = IAM
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse

    request = post "UpdateAccountPasswordPolicy"
    response _ = nullaryResponse UpdateAccountPasswordPolicyResponse
