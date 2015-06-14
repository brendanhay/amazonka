{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the password policy settings for the AWS account.
--
-- This action does not support partial updates. No parameters are
-- required, but if you do not specify a parameter, that parameter\'s value
-- reverts to its default value. See the __Request Parameters__ section for
-- each parameter\'s default value.
--
-- For more information about using a password policy, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccountPasswordPolicy.html>
module Network.AWS.IAM.UpdateAccountPasswordPolicy
    (
    -- * Request
      UpdateAccountPasswordPolicy
    -- ** Request constructor
    , updateAccountPasswordPolicy
    -- ** Request lenses
    , uappRequireNumbers
    , uappRequireLowercaseCharacters
    , uappHardExpiry
    , uappRequireSymbols
    , uappRequireUppercaseCharacters
    , uappAllowUsersToChangePassword
    , uappMinimumPasswordLength
    , uappPasswordReusePrevention
    , uappMaxPasswordAge

    -- * Response
    , UpdateAccountPasswordPolicyResponse
    -- ** Response constructor
    , updateAccountPasswordPolicyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'updateAccountPasswordPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uappRequireNumbers'
--
-- * 'uappRequireLowercaseCharacters'
--
-- * 'uappHardExpiry'
--
-- * 'uappRequireSymbols'
--
-- * 'uappRequireUppercaseCharacters'
--
-- * 'uappAllowUsersToChangePassword'
--
-- * 'uappMinimumPasswordLength'
--
-- * 'uappPasswordReusePrevention'
--
-- * 'uappMaxPasswordAge'
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy'{_uappRequireNumbers :: Maybe Bool, _uappRequireLowercaseCharacters :: Maybe Bool, _uappHardExpiry :: Maybe Bool, _uappRequireSymbols :: Maybe Bool, _uappRequireUppercaseCharacters :: Maybe Bool, _uappAllowUsersToChangePassword :: Maybe Bool, _uappMinimumPasswordLength :: Nat, _uappPasswordReusePrevention :: Nat, _uappMaxPasswordAge :: Nat} deriving (Eq, Read, Show)

-- | 'UpdateAccountPasswordPolicy' smart constructor.
updateAccountPasswordPolicy :: Natural -> Natural -> Natural -> UpdateAccountPasswordPolicy
updateAccountPasswordPolicy pMinimumPasswordLength pPasswordReusePrevention pMaxPasswordAge = UpdateAccountPasswordPolicy'{_uappRequireNumbers = Nothing, _uappRequireLowercaseCharacters = Nothing, _uappHardExpiry = Nothing, _uappRequireSymbols = Nothing, _uappRequireUppercaseCharacters = Nothing, _uappAllowUsersToChangePassword = Nothing, _uappMinimumPasswordLength = _Nat # pMinimumPasswordLength, _uappPasswordReusePrevention = _Nat # pPasswordReusePrevention, _uappMaxPasswordAge = _Nat # pMaxPasswordAge};

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
--
-- Default value: false
uappRequireNumbers :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireNumbers = lens _uappRequireNumbers (\ s a -> s{_uappRequireNumbers = a});

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
--
-- Default value: false
uappRequireLowercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireLowercaseCharacters = lens _uappRequireLowercaseCharacters (\ s a -> s{_uappRequireLowercaseCharacters = a});

-- | Prevents IAM users from setting a new password after their password has
-- expired.
--
-- Default value: false
uappHardExpiry :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappHardExpiry = lens _uappHardExpiry (\ s a -> s{_uappHardExpiry = a});

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters:
--
-- ! \@ # $ % ^ &amp; * ( ) _ + - = [ ] { } | \'
--
-- Default value: false
uappRequireSymbols :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireSymbols = lens _uappRequireSymbols (\ s a -> s{_uappRequireSymbols = a});

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
--
-- Default value: false
uappRequireUppercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireUppercaseCharacters = lens _uappRequireUppercaseCharacters (\ s a -> s{_uappRequireUppercaseCharacters = a});

-- | Allows all IAM users in your account to use the AWS Management Console
-- to change their own passwords. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords>
-- in the /Using IAM/ guide.
--
-- Default value: false
uappAllowUsersToChangePassword :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappAllowUsersToChangePassword = lens _uappAllowUsersToChangePassword (\ s a -> s{_uappAllowUsersToChangePassword = a});

-- | The minimum number of characters allowed in an IAM user password.
--
-- Default value: 6
uappMinimumPasswordLength :: Lens' UpdateAccountPasswordPolicy Natural
uappMinimumPasswordLength = lens _uappMinimumPasswordLength (\ s a -> s{_uappMinimumPasswordLength = a}) . _Nat;

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing. The default value of 0 means IAM users are not prevented
-- from reusing previous passwords.
--
-- Default value: 0
uappPasswordReusePrevention :: Lens' UpdateAccountPasswordPolicy Natural
uappPasswordReusePrevention = lens _uappPasswordReusePrevention (\ s a -> s{_uappPasswordReusePrevention = a}) . _Nat;

-- | The number of days that an IAM user password is valid. The default value
-- of 0 means IAM user passwords never expire.
--
-- Default value: 0
uappMaxPasswordAge :: Lens' UpdateAccountPasswordPolicy Natural
uappMaxPasswordAge = lens _uappMaxPasswordAge (\ s a -> s{_uappMaxPasswordAge = a}) . _Nat;

instance AWSRequest UpdateAccountPasswordPolicy where
        type Sv UpdateAccountPasswordPolicy = IAM
        type Rs UpdateAccountPasswordPolicy =
             UpdateAccountPasswordPolicyResponse
        request = post
        response
          = receiveNull UpdateAccountPasswordPolicyResponse'

instance ToHeaders UpdateAccountPasswordPolicy where
        toHeaders = const mempty

instance ToPath UpdateAccountPasswordPolicy where
        toPath = const "/"

instance ToQuery UpdateAccountPasswordPolicy where
        toQuery UpdateAccountPasswordPolicy'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAccountPasswordPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RequireNumbers" =: _uappRequireNumbers,
               "RequireLowercaseCharacters" =:
                 _uappRequireLowercaseCharacters,
               "HardExpiry" =: _uappHardExpiry,
               "RequireSymbols" =: _uappRequireSymbols,
               "RequireUppercaseCharacters" =:
                 _uappRequireUppercaseCharacters,
               "AllowUsersToChangePassword" =:
                 _uappAllowUsersToChangePassword,
               "MinimumPasswordLength" =:
                 _uappMinimumPasswordLength,
               "PasswordReusePrevention" =:
                 _uappPasswordReusePrevention,
               "MaxPasswordAge" =: _uappMaxPasswordAge]

-- | /See:/ 'updateAccountPasswordPolicyResponse' smart constructor.
data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse' deriving (Eq, Read, Show)

-- | 'UpdateAccountPasswordPolicyResponse' smart constructor.
updateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse
updateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse';
