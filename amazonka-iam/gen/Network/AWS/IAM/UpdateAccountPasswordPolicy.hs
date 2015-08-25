{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the password policy settings for the AWS account.
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccountPasswordPolicy.html AWS API Reference> for UpdateAccountPasswordPolicy.
module Network.AWS.IAM.UpdateAccountPasswordPolicy
    (
    -- * Creating a Request
      updateAccountPasswordPolicy
    , UpdateAccountPasswordPolicy
    -- * Request Lenses
    , uappRequireNumbers
    , uappMinimumPasswordLength
    , uappPasswordReusePrevention
    , uappRequireLowercaseCharacters
    , uappMaxPasswordAge
    , uappHardExpiry
    , uappRequireSymbols
    , uappRequireUppercaseCharacters
    , uappAllowUsersToChangePassword

    -- * Destructuring the Response
    , updateAccountPasswordPolicyResponse
    , UpdateAccountPasswordPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAccountPasswordPolicy' smart constructor.
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy'
    { _uappRequireNumbers             :: !(Maybe Bool)
    , _uappMinimumPasswordLength      :: !(Maybe Nat)
    , _uappPasswordReusePrevention    :: !(Maybe Nat)
    , _uappRequireLowercaseCharacters :: !(Maybe Bool)
    , _uappMaxPasswordAge             :: !(Maybe Nat)
    , _uappHardExpiry                 :: !(Maybe Bool)
    , _uappRequireSymbols             :: !(Maybe Bool)
    , _uappRequireUppercaseCharacters :: !(Maybe Bool)
    , _uappAllowUsersToChangePassword :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAccountPasswordPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uappRequireNumbers'
--
-- * 'uappMinimumPasswordLength'
--
-- * 'uappPasswordReusePrevention'
--
-- * 'uappRequireLowercaseCharacters'
--
-- * 'uappMaxPasswordAge'
--
-- * 'uappHardExpiry'
--
-- * 'uappRequireSymbols'
--
-- * 'uappRequireUppercaseCharacters'
--
-- * 'uappAllowUsersToChangePassword'
updateAccountPasswordPolicy
    :: UpdateAccountPasswordPolicy
updateAccountPasswordPolicy =
    UpdateAccountPasswordPolicy'
    { _uappRequireNumbers = Nothing
    , _uappMinimumPasswordLength = Nothing
    , _uappPasswordReusePrevention = Nothing
    , _uappRequireLowercaseCharacters = Nothing
    , _uappMaxPasswordAge = Nothing
    , _uappHardExpiry = Nothing
    , _uappRequireSymbols = Nothing
    , _uappRequireUppercaseCharacters = Nothing
    , _uappAllowUsersToChangePassword = Nothing
    }

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
--
-- Default value: false
uappRequireNumbers :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireNumbers = lens _uappRequireNumbers (\ s a -> s{_uappRequireNumbers = a});

-- | The minimum number of characters allowed in an IAM user password.
--
-- Default value: 6
uappMinimumPasswordLength :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uappMinimumPasswordLength = lens _uappMinimumPasswordLength (\ s a -> s{_uappMinimumPasswordLength = a}) . mapping _Nat;

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing. The default value of 0 means IAM users are not prevented
-- from reusing previous passwords.
--
-- Default value: 0
uappPasswordReusePrevention :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uappPasswordReusePrevention = lens _uappPasswordReusePrevention (\ s a -> s{_uappPasswordReusePrevention = a}) . mapping _Nat;

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
--
-- Default value: false
uappRequireLowercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireLowercaseCharacters = lens _uappRequireLowercaseCharacters (\ s a -> s{_uappRequireLowercaseCharacters = a});

-- | The number of days that an IAM user password is valid. The default value
-- of 0 means IAM user passwords never expire.
--
-- Default value: 0
uappMaxPasswordAge :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uappMaxPasswordAge = lens _uappMaxPasswordAge (\ s a -> s{_uappMaxPasswordAge = a}) . mapping _Nat;

-- | Prevents IAM users from setting a new password after their password has
-- expired.
--
-- Default value: false
uappHardExpiry :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappHardExpiry = lens _uappHardExpiry (\ s a -> s{_uappHardExpiry = a});

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters:
--
-- ! \' # $ % ^ &amp; * ( ) _ + - = [ ] { } | \'
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

instance AWSRequest UpdateAccountPasswordPolicy where
        type Rs UpdateAccountPasswordPolicy =
             UpdateAccountPasswordPolicyResponse
        request = postQuery iAM
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
               "MinimumPasswordLength" =:
                 _uappMinimumPasswordLength,
               "PasswordReusePrevention" =:
                 _uappPasswordReusePrevention,
               "RequireLowercaseCharacters" =:
                 _uappRequireLowercaseCharacters,
               "MaxPasswordAge" =: _uappMaxPasswordAge,
               "HardExpiry" =: _uappHardExpiry,
               "RequireSymbols" =: _uappRequireSymbols,
               "RequireUppercaseCharacters" =:
                 _uappRequireUppercaseCharacters,
               "AllowUsersToChangePassword" =:
                 _uappAllowUsersToChangePassword]

-- | /See:/ 'updateAccountPasswordPolicyResponse' smart constructor.
data UpdateAccountPasswordPolicyResponse =
    UpdateAccountPasswordPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAccountPasswordPolicyResponse' with the minimum fields required to make a request.
--
updateAccountPasswordPolicyResponse
    :: UpdateAccountPasswordPolicyResponse
updateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse'
