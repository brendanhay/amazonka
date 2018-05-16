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
-- Module      : Network.AWS.SecretsManager.GetRandomPassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a random password of the specified complexity. This operation is intended for use in the Lambda rotation function. Per best practice, we recommend that you specify the maximum length and include every character type that the system you are generating a password for can support.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetRandomPassword
--
--
--
module Network.AWS.SecretsManager.GetRandomPassword
    (
    -- * Creating a Request
      getRandomPassword
    , GetRandomPassword
    -- * Request Lenses
    , grpIncludeSpace
    , grpExcludeNumbers
    , grpExcludeLowercase
    , grpExcludeCharacters
    , grpExcludePunctuation
    , grpRequireEachIncludedType
    , grpExcludeUppercase
    , grpPasswordLength

    -- * Destructuring the Response
    , getRandomPasswordResponse
    , GetRandomPasswordResponse
    -- * Response Lenses
    , grprsRandomPassword
    , grprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'getRandomPassword' smart constructor.
data GetRandomPassword = GetRandomPassword'
  { _grpIncludeSpace            :: !(Maybe Bool)
  , _grpExcludeNumbers          :: !(Maybe Bool)
  , _grpExcludeLowercase        :: !(Maybe Bool)
  , _grpExcludeCharacters       :: !(Maybe Text)
  , _grpExcludePunctuation      :: !(Maybe Bool)
  , _grpRequireEachIncludedType :: !(Maybe Bool)
  , _grpExcludeUppercase        :: !(Maybe Bool)
  , _grpPasswordLength          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRandomPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpIncludeSpace' - Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
--
-- * 'grpExcludeNumbers' - Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
--
-- * 'grpExcludeLowercase' - Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
--
-- * 'grpExcludeCharacters' - A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
--
-- * 'grpExcludePunctuation' - Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
--
-- * 'grpRequireEachIncludedType' - A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is @True@ and the operation requires at least one of every character type.
--
-- * 'grpExcludeUppercase' - Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
--
-- * 'grpPasswordLength' - The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
getRandomPassword
    :: GetRandomPassword
getRandomPassword =
  GetRandomPassword'
    { _grpIncludeSpace = Nothing
    , _grpExcludeNumbers = Nothing
    , _grpExcludeLowercase = Nothing
    , _grpExcludeCharacters = Nothing
    , _grpExcludePunctuation = Nothing
    , _grpRequireEachIncludedType = Nothing
    , _grpExcludeUppercase = Nothing
    , _grpPasswordLength = Nothing
    }


-- | Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
grpIncludeSpace :: Lens' GetRandomPassword (Maybe Bool)
grpIncludeSpace = lens _grpIncludeSpace (\ s a -> s{_grpIncludeSpace = a})

-- | Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
grpExcludeNumbers :: Lens' GetRandomPassword (Maybe Bool)
grpExcludeNumbers = lens _grpExcludeNumbers (\ s a -> s{_grpExcludeNumbers = a})

-- | Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
grpExcludeLowercase :: Lens' GetRandomPassword (Maybe Bool)
grpExcludeLowercase = lens _grpExcludeLowercase (\ s a -> s{_grpExcludeLowercase = a})

-- | A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
grpExcludeCharacters :: Lens' GetRandomPassword (Maybe Text)
grpExcludeCharacters = lens _grpExcludeCharacters (\ s a -> s{_grpExcludeCharacters = a})

-- | Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
grpExcludePunctuation :: Lens' GetRandomPassword (Maybe Bool)
grpExcludePunctuation = lens _grpExcludePunctuation (\ s a -> s{_grpExcludePunctuation = a})

-- | A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is @True@ and the operation requires at least one of every character type.
grpRequireEachIncludedType :: Lens' GetRandomPassword (Maybe Bool)
grpRequireEachIncludedType = lens _grpRequireEachIncludedType (\ s a -> s{_grpRequireEachIncludedType = a})

-- | Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
grpExcludeUppercase :: Lens' GetRandomPassword (Maybe Bool)
grpExcludeUppercase = lens _grpExcludeUppercase (\ s a -> s{_grpExcludeUppercase = a})

-- | The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
grpPasswordLength :: Lens' GetRandomPassword (Maybe Natural)
grpPasswordLength = lens _grpPasswordLength (\ s a -> s{_grpPasswordLength = a}) . mapping _Nat

instance AWSRequest GetRandomPassword where
        type Rs GetRandomPassword = GetRandomPasswordResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 GetRandomPasswordResponse' <$>
                   (x .?> "RandomPassword") <*> (pure (fromEnum s)))

instance Hashable GetRandomPassword where

instance NFData GetRandomPassword where

instance ToHeaders GetRandomPassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.GetRandomPassword" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRandomPassword where
        toJSON GetRandomPassword'{..}
          = object
              (catMaybes
                 [("IncludeSpace" .=) <$> _grpIncludeSpace,
                  ("ExcludeNumbers" .=) <$> _grpExcludeNumbers,
                  ("ExcludeLowercase" .=) <$> _grpExcludeLowercase,
                  ("ExcludeCharacters" .=) <$> _grpExcludeCharacters,
                  ("ExcludePunctuation" .=) <$> _grpExcludePunctuation,
                  ("RequireEachIncludedType" .=) <$>
                    _grpRequireEachIncludedType,
                  ("ExcludeUppercase" .=) <$> _grpExcludeUppercase,
                  ("PasswordLength" .=) <$> _grpPasswordLength])

instance ToPath GetRandomPassword where
        toPath = const "/"

instance ToQuery GetRandomPassword where
        toQuery = const mempty

-- | /See:/ 'getRandomPasswordResponse' smart constructor.
data GetRandomPasswordResponse = GetRandomPasswordResponse'
  { _grprsRandomPassword :: !(Maybe Text)
  , _grprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRandomPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsRandomPassword' - A string with the generated password.
--
-- * 'grprsResponseStatus' - -- | The response status code.
getRandomPasswordResponse
    :: Int -- ^ 'grprsResponseStatus'
    -> GetRandomPasswordResponse
getRandomPasswordResponse pResponseStatus_ =
  GetRandomPasswordResponse'
    {_grprsRandomPassword = Nothing, _grprsResponseStatus = pResponseStatus_}


-- | A string with the generated password.
grprsRandomPassword :: Lens' GetRandomPasswordResponse (Maybe Text)
grprsRandomPassword = lens _grprsRandomPassword (\ s a -> s{_grprsRandomPassword = a})

-- | -- | The response status code.
grprsResponseStatus :: Lens' GetRandomPasswordResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\ s a -> s{_grprsResponseStatus = a})

instance NFData GetRandomPasswordResponse where
