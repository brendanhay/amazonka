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
-- Module      : Network.AWS.SecretsManager.GetSecretValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of the encrypted fields @SecretString@ or @SecretBinary@ from the specified version of a secret, whichever contains content.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetSecretValue
--
--     * kms:Decrypt - required only if you use a customer-created KMS key to encrypt the secret. You do not need this permission to use the account's default AWS managed CMK for Secrets Manager.
--
--
--
-- __Related operations__
--
--     * To create a new version of the secret with different encrypted information, use 'PutSecretValue' .
--
--     * To retrieve the non-encrypted details for the secret, use 'DescribeSecret' .
--
--
--
module Network.AWS.SecretsManager.GetSecretValue
    (
    -- * Creating a Request
      getSecretValue
    , GetSecretValue
    -- * Request Lenses
    , gsvVersionId
    , gsvVersionStage
    , gsvSecretId

    -- * Destructuring the Response
    , getSecretValueResponse
    , GetSecretValueResponse
    -- * Response Lenses
    , gsvrsVersionId
    , gsvrsARN
    , gsvrsVersionStages
    , gsvrsSecretBinary
    , gsvrsCreatedDate
    , gsvrsName
    , gsvrsSecretString
    , gsvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'getSecretValue' smart constructor.
data GetSecretValue = GetSecretValue'
  { _gsvVersionId    :: !(Maybe Text)
  , _gsvVersionStage :: !(Maybe Text)
  , _gsvSecretId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSecretValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvVersionId' - Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @SecretVersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ . This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
--
-- * 'gsvVersionStage' - Specifies the secret version that you want to retrieve by the staging label attached to the version. Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @SecretVersionId@ . If you don't specify either a @VersionStage@ or @SecretVersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
--
-- * 'gsvSecretId' - Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
getSecretValue
    :: Text -- ^ 'gsvSecretId'
    -> GetSecretValue
getSecretValue pSecretId_ =
  GetSecretValue'
    { _gsvVersionId = Nothing
    , _gsvVersionStage = Nothing
    , _gsvSecretId = pSecretId_
    }


-- | Specifies the unique identifier of the version of the secret that you want to retrieve. If you specify this parameter then don't specify @VersionStage@ . If you don't specify either a @VersionStage@ or @SecretVersionId@ then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ . This value is typically a <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type> value with 32 hexadecimal digits.
gsvVersionId :: Lens' GetSecretValue (Maybe Text)
gsvVersionId = lens _gsvVersionId (\ s a -> s{_gsvVersionId = a})

-- | Specifies the secret version that you want to retrieve by the staging label attached to the version. Staging labels are used to keep track of different versions during the rotation process. If you use this parameter then don't specify @SecretVersionId@ . If you don't specify either a @VersionStage@ or @SecretVersionId@ , then the default is to perform the operation on the version with the @VersionStage@ value of @AWSCURRENT@ .
gsvVersionStage :: Lens' GetSecretValue (Maybe Text)
gsvVersionStage = lens _gsvVersionStage (\ s a -> s{_gsvVersionStage = a})

-- | Specifies the secret containing the version that you want to retrieve. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
gsvSecretId :: Lens' GetSecretValue Text
gsvSecretId = lens _gsvSecretId (\ s a -> s{_gsvSecretId = a})

instance AWSRequest GetSecretValue where
        type Rs GetSecretValue = GetSecretValueResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 GetSecretValueResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "ARN") <*>
                     (x .?> "VersionStages")
                     <*> (x .?> "SecretBinary")
                     <*> (x .?> "CreatedDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "SecretString")
                     <*> (pure (fromEnum s)))

instance Hashable GetSecretValue where

instance NFData GetSecretValue where

instance ToHeaders GetSecretValue where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.GetSecretValue" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSecretValue where
        toJSON GetSecretValue'{..}
          = object
              (catMaybes
                 [("VersionId" .=) <$> _gsvVersionId,
                  ("VersionStage" .=) <$> _gsvVersionStage,
                  Just ("SecretId" .= _gsvSecretId)])

instance ToPath GetSecretValue where
        toPath = const "/"

instance ToQuery GetSecretValue where
        toQuery = const mempty

-- | /See:/ 'getSecretValueResponse' smart constructor.
data GetSecretValueResponse = GetSecretValueResponse'
  { _gsvrsVersionId      :: !(Maybe Text)
  , _gsvrsARN            :: !(Maybe Text)
  , _gsvrsVersionStages  :: !(Maybe (List1 Text))
  , _gsvrsSecretBinary   :: !(Maybe (Sensitive Base64))
  , _gsvrsCreatedDate    :: !(Maybe POSIX)
  , _gsvrsName           :: !(Maybe Text)
  , _gsvrsSecretString   :: !(Maybe (Sensitive Text))
  , _gsvrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSecretValueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvrsVersionId' - The unique identifier of this version of the secret.
--
-- * 'gsvrsARN' - The ARN of the secret.
--
-- * 'gsvrsVersionStages' - A list of all of the staging labels currently attached to this version of the secret.
--
-- * 'gsvrsSecretBinary' - The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string. This parameter is not used if the secret is created by the Secrets Manager console. If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gsvrsCreatedDate' - The date and time that this version of the secret was created.
--
-- * 'gsvrsName' - The friendly name of the secret.
--
-- * 'gsvrsSecretString' - The decrypted part of the protected secret information that was originally provided as a string. If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse. If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
--
-- * 'gsvrsResponseStatus' - -- | The response status code.
getSecretValueResponse
    :: Int -- ^ 'gsvrsResponseStatus'
    -> GetSecretValueResponse
getSecretValueResponse pResponseStatus_ =
  GetSecretValueResponse'
    { _gsvrsVersionId = Nothing
    , _gsvrsARN = Nothing
    , _gsvrsVersionStages = Nothing
    , _gsvrsSecretBinary = Nothing
    , _gsvrsCreatedDate = Nothing
    , _gsvrsName = Nothing
    , _gsvrsSecretString = Nothing
    , _gsvrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier of this version of the secret.
gsvrsVersionId :: Lens' GetSecretValueResponse (Maybe Text)
gsvrsVersionId = lens _gsvrsVersionId (\ s a -> s{_gsvrsVersionId = a})

-- | The ARN of the secret.
gsvrsARN :: Lens' GetSecretValueResponse (Maybe Text)
gsvrsARN = lens _gsvrsARN (\ s a -> s{_gsvrsARN = a})

-- | A list of all of the staging labels currently attached to this version of the secret.
gsvrsVersionStages :: Lens' GetSecretValueResponse (Maybe (NonEmpty Text))
gsvrsVersionStages = lens _gsvrsVersionStages (\ s a -> s{_gsvrsVersionStages = a}) . mapping _List1

-- | The decrypted part of the protected secret information that was originally provided as binary data in the form of a byte array. The response parameter represents the binary data as a <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string. This parameter is not used if the secret is created by the Secrets Manager console. If you store custom information in this field of the secret, then you must code your Lambda rotation function to parse and interpret whatever you store in the @SecretString@ or @SecretBinary@ fields.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gsvrsSecretBinary :: Lens' GetSecretValueResponse (Maybe ByteString)
gsvrsSecretBinary = lens _gsvrsSecretBinary (\ s a -> s{_gsvrsSecretBinary = a}) . mapping (_Sensitive . _Base64)

-- | The date and time that this version of the secret was created.
gsvrsCreatedDate :: Lens' GetSecretValueResponse (Maybe UTCTime)
gsvrsCreatedDate = lens _gsvrsCreatedDate (\ s a -> s{_gsvrsCreatedDate = a}) . mapping _Time

-- | The friendly name of the secret.
gsvrsName :: Lens' GetSecretValueResponse (Maybe Text)
gsvrsName = lens _gsvrsName (\ s a -> s{_gsvrsName = a})

-- | The decrypted part of the protected secret information that was originally provided as a string. If you create this secret by using the Secrets Manager console then only the @SecretString@ parameter contains data. Secrets Manager stores the information as a JSON structure of key/value pairs that the Lambda rotation function knows how to parse. If you store custom information in the secret by using the 'CreateSecret' , 'UpdateSecret' , or 'PutSecretValue' API operations instead of the Secrets Manager console, or by using the __Other secret type__ in the console, then you must code your Lambda rotation function to parse and interpret those values.
gsvrsSecretString :: Lens' GetSecretValueResponse (Maybe Text)
gsvrsSecretString = lens _gsvrsSecretString (\ s a -> s{_gsvrsSecretString = a}) . mapping _Sensitive

-- | -- | The response status code.
gsvrsResponseStatus :: Lens' GetSecretValueResponse Int
gsvrsResponseStatus = lens _gsvrsResponseStatus (\ s a -> s{_gsvrsResponseStatus = a})

instance NFData GetSecretValueResponse where
