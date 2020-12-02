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
-- Module      : Network.AWS.KMS.GetParametersForImport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the items you need in order to import key material into AWS KMS from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ .
--
--
-- You must specify the key ID of the customer master key (CMK) into which you will import key material. This CMK's @Origin@ must be @EXTERNAL@ . You must also specify the wrapping algorithm and type of wrapping key (public key) that you will use to encrypt the key material. You cannot perform this operation on a CMK in a different AWS account.
--
-- This operation returns a public key and an import token. Use the public key to encrypt the key material. Store the import token to send with a subsequent 'ImportKeyMaterial' request. The public key and import token from the same response must be used together. These items are valid for 24 hours. When they expire, they cannot be used for a subsequent 'ImportKeyMaterial' request. To get new ones, send another @GetParametersForImport@ request.
--
module Network.AWS.KMS.GetParametersForImport
    (
    -- * Creating a Request
      getParametersForImport
    , GetParametersForImport
    -- * Request Lenses
    , gpfiKeyId
    , gpfiWrappingAlgorithm
    , gpfiWrappingKeySpec

    -- * Destructuring the Response
    , getParametersForImportResponse
    , GetParametersForImportResponse
    -- * Response Lenses
    , gpfirsKeyId
    , gpfirsPublicKey
    , gpfirsParametersValidTo
    , gpfirsImportToken
    , gpfirsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getParametersForImport' smart constructor.
data GetParametersForImport = GetParametersForImport'
  { _gpfiKeyId             :: !Text
  , _gpfiWrappingAlgorithm :: !AlgorithmSpec
  , _gpfiWrappingKeySpec   :: !WrappingKeySpec
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetParametersForImport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpfiKeyId' - The identifier of the CMK into which you will import key material. The CMK's @Origin@ must be @EXTERNAL@ . Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- * 'gpfiWrappingAlgorithm' - The algorithm you will use to encrypt the key material before importing it with 'ImportKeyMaterial' . For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gpfiWrappingKeySpec' - The type of wrapping key (public key) to return in the response. Only 2048-bit RSA public keys are supported.
getParametersForImport
    :: Text -- ^ 'gpfiKeyId'
    -> AlgorithmSpec -- ^ 'gpfiWrappingAlgorithm'
    -> WrappingKeySpec -- ^ 'gpfiWrappingKeySpec'
    -> GetParametersForImport
getParametersForImport pKeyId_ pWrappingAlgorithm_ pWrappingKeySpec_ =
  GetParametersForImport'
    { _gpfiKeyId = pKeyId_
    , _gpfiWrappingAlgorithm = pWrappingAlgorithm_
    , _gpfiWrappingKeySpec = pWrappingKeySpec_
    }


-- | The identifier of the CMK into which you will import key material. The CMK's @Origin@ must be @EXTERNAL@ . Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
gpfiKeyId :: Lens' GetParametersForImport Text
gpfiKeyId = lens _gpfiKeyId (\ s a -> s{_gpfiKeyId = a})

-- | The algorithm you will use to encrypt the key material before importing it with 'ImportKeyMaterial' . For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material> in the /AWS Key Management Service Developer Guide/ .
gpfiWrappingAlgorithm :: Lens' GetParametersForImport AlgorithmSpec
gpfiWrappingAlgorithm = lens _gpfiWrappingAlgorithm (\ s a -> s{_gpfiWrappingAlgorithm = a})

-- | The type of wrapping key (public key) to return in the response. Only 2048-bit RSA public keys are supported.
gpfiWrappingKeySpec :: Lens' GetParametersForImport WrappingKeySpec
gpfiWrappingKeySpec = lens _gpfiWrappingKeySpec (\ s a -> s{_gpfiWrappingKeySpec = a})

instance AWSRequest GetParametersForImport where
        type Rs GetParametersForImport =
             GetParametersForImportResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 GetParametersForImportResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "PublicKey") <*>
                     (x .?> "ParametersValidTo")
                     <*> (x .?> "ImportToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetParametersForImport where

instance NFData GetParametersForImport where

instance ToHeaders GetParametersForImport where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GetParametersForImport" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetParametersForImport where
        toJSON GetParametersForImport'{..}
          = object
              (catMaybes
                 [Just ("KeyId" .= _gpfiKeyId),
                  Just ("WrappingAlgorithm" .= _gpfiWrappingAlgorithm),
                  Just ("WrappingKeySpec" .= _gpfiWrappingKeySpec)])

instance ToPath GetParametersForImport where
        toPath = const "/"

instance ToQuery GetParametersForImport where
        toQuery = const mempty

-- | /See:/ 'getParametersForImportResponse' smart constructor.
data GetParametersForImportResponse = GetParametersForImportResponse'
  { _gpfirsKeyId             :: !(Maybe Text)
  , _gpfirsPublicKey         :: !(Maybe (Sensitive Base64))
  , _gpfirsParametersValidTo :: !(Maybe POSIX)
  , _gpfirsImportToken       :: !(Maybe Base64)
  , _gpfirsResponseStatus    :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetParametersForImportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpfirsKeyId' - The identifier of the CMK to use in a subsequent 'ImportKeyMaterial' request. This is the same CMK specified in the @GetParametersForImport@ request.
--
-- * 'gpfirsPublicKey' - The public key to use to encrypt the key material before importing it with 'ImportKeyMaterial' .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gpfirsParametersValidTo' - The time at which the import token and public key are no longer valid. After this time, you cannot use them to make an 'ImportKeyMaterial' request and you must send another @GetParametersForImport@ request to get new ones.
--
-- * 'gpfirsImportToken' - The import token to send in a subsequent 'ImportKeyMaterial' request.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gpfirsResponseStatus' - -- | The response status code.
getParametersForImportResponse
    :: Int -- ^ 'gpfirsResponseStatus'
    -> GetParametersForImportResponse
getParametersForImportResponse pResponseStatus_ =
  GetParametersForImportResponse'
    { _gpfirsKeyId = Nothing
    , _gpfirsPublicKey = Nothing
    , _gpfirsParametersValidTo = Nothing
    , _gpfirsImportToken = Nothing
    , _gpfirsResponseStatus = pResponseStatus_
    }


-- | The identifier of the CMK to use in a subsequent 'ImportKeyMaterial' request. This is the same CMK specified in the @GetParametersForImport@ request.
gpfirsKeyId :: Lens' GetParametersForImportResponse (Maybe Text)
gpfirsKeyId = lens _gpfirsKeyId (\ s a -> s{_gpfirsKeyId = a})

-- | The public key to use to encrypt the key material before importing it with 'ImportKeyMaterial' .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gpfirsPublicKey :: Lens' GetParametersForImportResponse (Maybe ByteString)
gpfirsPublicKey = lens _gpfirsPublicKey (\ s a -> s{_gpfirsPublicKey = a}) . mapping (_Sensitive . _Base64)

-- | The time at which the import token and public key are no longer valid. After this time, you cannot use them to make an 'ImportKeyMaterial' request and you must send another @GetParametersForImport@ request to get new ones.
gpfirsParametersValidTo :: Lens' GetParametersForImportResponse (Maybe UTCTime)
gpfirsParametersValidTo = lens _gpfirsParametersValidTo (\ s a -> s{_gpfirsParametersValidTo = a}) . mapping _Time

-- | The import token to send in a subsequent 'ImportKeyMaterial' request.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gpfirsImportToken :: Lens' GetParametersForImportResponse (Maybe ByteString)
gpfirsImportToken = lens _gpfirsImportToken (\ s a -> s{_gpfirsImportToken = a}) . mapping _Base64

-- | -- | The response status code.
gpfirsResponseStatus :: Lens' GetParametersForImportResponse Int
gpfirsResponseStatus = lens _gpfirsResponseStatus (\ s a -> s{_gpfirsResponseStatus = a})

instance NFData GetParametersForImportResponse where
