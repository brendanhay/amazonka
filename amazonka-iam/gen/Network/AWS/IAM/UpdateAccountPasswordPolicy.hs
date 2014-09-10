{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
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
module Network.AWS.IAM.UpdateAccountPasswordPolicy
    (
    -- * Request
      UpdateAccountPasswordPolicy
    -- ** Request constructor
    , mkUpdateAccountPasswordPolicy
    -- ** Request lenses
    , uappMinimumPasswordLength
    , uappRequireSymbols
    , uappRequireNumbers
    , uappRequireUppercaseCharacters
    , uappRequireLowercaseCharacters
    , uappAllowUsersToChangePassword
    , uappMaxPasswordAge
    , uappPasswordReusePrevention
    , uappHardExpiry

    -- * Response
    , UpdateAccountPasswordPolicyResponse
    -- ** Response constructor
    , mkUpdateAccountPasswordPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uappMinimumPasswordLength :: !(Maybe Integer)
    , _uappRequireSymbols :: !(Maybe Bool)
    , _uappRequireNumbers :: !(Maybe Bool)
    , _uappRequireUppercaseCharacters :: !(Maybe Bool)
    , _uappRequireLowercaseCharacters :: !(Maybe Bool)
    , _uappAllowUsersToChangePassword :: !(Maybe Bool)
    , _uappMaxPasswordAge :: !(Maybe Integer)
    , _uappPasswordReusePrevention :: !(Maybe Integer)
    , _uappHardExpiry :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAccountPasswordPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MinimumPasswordLength ::@ @Maybe Integer@
--
-- * @RequireSymbols ::@ @Maybe Bool@
--
-- * @RequireNumbers ::@ @Maybe Bool@
--
-- * @RequireUppercaseCharacters ::@ @Maybe Bool@
--
-- * @RequireLowercaseCharacters ::@ @Maybe Bool@
--
-- * @AllowUsersToChangePassword ::@ @Maybe Bool@
--
-- * @MaxPasswordAge ::@ @Maybe Integer@
--
-- * @PasswordReusePrevention ::@ @Maybe Integer@
--
-- * @HardExpiry ::@ @Maybe Bool@
--
mkUpdateAccountPasswordPolicy :: UpdateAccountPasswordPolicy
mkUpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uappMinimumPasswordLength = Nothing
    , _uappRequireSymbols = Nothing
    , _uappRequireNumbers = Nothing
    , _uappRequireUppercaseCharacters = Nothing
    , _uappRequireLowercaseCharacters = Nothing
    , _uappAllowUsersToChangePassword = Nothing
    , _uappMaxPasswordAge = Nothing
    , _uappPasswordReusePrevention = Nothing
    , _uappHardExpiry = Nothing
    }

-- | The minimum number of characters allowed in an IAM user password.
uappMinimumPasswordLength :: Lens' UpdateAccountPasswordPolicy (Maybe Integer)
uappMinimumPasswordLength =
    lens _uappMinimumPasswordLength
         (\s a -> s { _uappMinimumPasswordLength = a })

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters: ! @ # $ % ^ &amp; * ( ) _ + - = [ ]
-- { } | '.
uappRequireSymbols :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireSymbols =
    lens _uappRequireSymbols (\s a -> s { _uappRequireSymbols = a })

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
uappRequireNumbers :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireNumbers =
    lens _uappRequireNumbers (\s a -> s { _uappRequireNumbers = a })

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
uappRequireUppercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireUppercaseCharacters =
    lens _uappRequireUppercaseCharacters
         (\s a -> s { _uappRequireUppercaseCharacters = a })

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
uappRequireLowercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireLowercaseCharacters =
    lens _uappRequireLowercaseCharacters
         (\s a -> s { _uappRequireLowercaseCharacters = a })

-- | Allows all IAM users in your account to use the AWS Management Console to
-- change their own passwords. For more information, see Letting IAM Users
-- Change Their Own Passwords in the Using IAM guide.
uappAllowUsersToChangePassword :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappAllowUsersToChangePassword =
    lens _uappAllowUsersToChangePassword
         (\s a -> s { _uappAllowUsersToChangePassword = a })

-- | The number of days that an IAM user password is valid.
uappMaxPasswordAge :: Lens' UpdateAccountPasswordPolicy (Maybe Integer)
uappMaxPasswordAge =
    lens _uappMaxPasswordAge (\s a -> s { _uappMaxPasswordAge = a })

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
uappPasswordReusePrevention :: Lens' UpdateAccountPasswordPolicy (Maybe Integer)
uappPasswordReusePrevention =
    lens _uappPasswordReusePrevention
         (\s a -> s { _uappPasswordReusePrevention = a })

-- | Prevents IAM users from setting a new password after their password has
-- expired.
uappHardExpiry :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappHardExpiry = lens _uappHardExpiry (\s a -> s { _uappHardExpiry = a })

instance ToQuery UpdateAccountPasswordPolicy where
    toQuery = genericQuery def

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAccountPasswordPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse
mkUpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse

instance AWSRequest UpdateAccountPasswordPolicy where
    type Sv UpdateAccountPasswordPolicy = IAM
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse

    request = post "UpdateAccountPasswordPolicy"
    response _ = nullaryResponse UpdateAccountPasswordPolicyResponse
