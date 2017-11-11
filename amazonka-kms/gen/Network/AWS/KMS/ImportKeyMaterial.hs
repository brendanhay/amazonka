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
-- Module      : Network.AWS.KMS.ImportKeyMaterial
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports key material into an AWS KMS customer master key (CMK) from your existing key management infrastructure. For more information about importing key material into AWS KMS, see <http://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ .
--
--
-- You must specify the key ID of the CMK to import the key material into. This CMK's @Origin@ must be @EXTERNAL@ . You must also send an import token and the encrypted key material. Send the import token that you received in the same 'GetParametersForImport' response that contained the public key that you used to encrypt the key material. You must also specify whether the key material expires and if so, when. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. To use the CMK again, you can reimport the same key material. If you set an expiration date, you can change it only by reimporting the same key material and specifying a new expiration date.
--
-- When this operation is successful, the specified CMK's key state changes to @Enabled@ , and you can use the CMK.
--
-- After you successfully import key material into a CMK, you can reimport the same key material into that CMK, but you cannot import different key material.
--
module Network.AWS.KMS.ImportKeyMaterial
    (
    -- * Creating a Request
      importKeyMaterial
    , ImportKeyMaterial
    -- * Request Lenses
    , ikmExpirationModel
    , ikmValidTo
    , ikmKeyId
    , ikmImportToken
    , ikmEncryptedKeyMaterial

    -- * Destructuring the Response
    , importKeyMaterialResponse
    , ImportKeyMaterialResponse
    -- * Response Lenses
    , ikmrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { _ikmExpirationModel      :: {-# NOUNPACK #-}!(Maybe ExpirationModelType)
  , _ikmValidTo              :: {-# NOUNPACK #-}!(Maybe POSIX)
  , _ikmKeyId                :: {-# NOUNPACK #-}!Text
  , _ikmImportToken          :: {-# NOUNPACK #-}!Base64
  , _ikmEncryptedKeyMaterial :: {-# NOUNPACK #-}!Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportKeyMaterial' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikmExpirationModel' - Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
--
-- * 'ikmValidTo' - The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
--
-- * 'ikmKeyId' - The identifier of the CMK to import the key material into. The CMK's @Origin@ must be @EXTERNAL@ . A valid identifier is the unique key ID or the Amazon Resource Name (ARN) of the CMK. Examples:     * Unique key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- * 'ikmImportToken' - The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'ikmEncryptedKeyMaterial' - The encrypted key material to import. It must be encrypted with the public key that you received in the response to a previous 'GetParametersForImport' request, using the wrapping algorithm that you specified in that request.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
importKeyMaterial
    :: Text -- ^ 'ikmKeyId'
    -> ByteString -- ^ 'ikmImportToken'
    -> ByteString -- ^ 'ikmEncryptedKeyMaterial'
    -> ImportKeyMaterial
importKeyMaterial pKeyId_ pImportToken_ pEncryptedKeyMaterial_ =
  ImportKeyMaterial'
  { _ikmExpirationModel = Nothing
  , _ikmValidTo = Nothing
  , _ikmKeyId = pKeyId_
  , _ikmImportToken = _Base64 # pImportToken_
  , _ikmEncryptedKeyMaterial = _Base64 # pEncryptedKeyMaterial_
  }


-- | Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
ikmExpirationModel :: Lens' ImportKeyMaterial (Maybe ExpirationModelType)
ikmExpirationModel = lens _ikmExpirationModel (\ s a -> s{_ikmExpirationModel = a});

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
ikmValidTo :: Lens' ImportKeyMaterial (Maybe UTCTime)
ikmValidTo = lens _ikmValidTo (\ s a -> s{_ikmValidTo = a}) . mapping _Time;

-- | The identifier of the CMK to import the key material into. The CMK's @Origin@ must be @EXTERNAL@ . A valid identifier is the unique key ID or the Amazon Resource Name (ARN) of the CMK. Examples:     * Unique key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
ikmKeyId :: Lens' ImportKeyMaterial Text
ikmKeyId = lens _ikmKeyId (\ s a -> s{_ikmKeyId = a});

-- | The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ikmImportToken :: Lens' ImportKeyMaterial ByteString
ikmImportToken = lens _ikmImportToken (\ s a -> s{_ikmImportToken = a}) . _Base64;

-- | The encrypted key material to import. It must be encrypted with the public key that you received in the response to a previous 'GetParametersForImport' request, using the wrapping algorithm that you specified in that request.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ikmEncryptedKeyMaterial :: Lens' ImportKeyMaterial ByteString
ikmEncryptedKeyMaterial = lens _ikmEncryptedKeyMaterial (\ s a -> s{_ikmEncryptedKeyMaterial = a}) . _Base64;

instance AWSRequest ImportKeyMaterial where
        type Rs ImportKeyMaterial = ImportKeyMaterialResponse
        request = postJSON kms
        response
          = receiveEmpty
              (\ s h x ->
                 ImportKeyMaterialResponse' <$> (pure (fromEnum s)))

instance Hashable ImportKeyMaterial where

instance NFData ImportKeyMaterial where

instance ToHeaders ImportKeyMaterial where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ImportKeyMaterial" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportKeyMaterial where
        toJSON ImportKeyMaterial'{..}
          = object
              (catMaybes
                 [("ExpirationModel" .=) <$> _ikmExpirationModel,
                  ("ValidTo" .=) <$> _ikmValidTo,
                  Just ("KeyId" .= _ikmKeyId),
                  Just ("ImportToken" .= _ikmImportToken),
                  Just
                    ("EncryptedKeyMaterial" .=
                       _ikmEncryptedKeyMaterial)])

instance ToPath ImportKeyMaterial where
        toPath = const "/"

instance ToQuery ImportKeyMaterial where
        toQuery = const mempty

-- | /See:/ 'importKeyMaterialResponse' smart constructor.
newtype ImportKeyMaterialResponse = ImportKeyMaterialResponse'
  { _ikmrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportKeyMaterialResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikmrsResponseStatus' - -- | The response status code.
importKeyMaterialResponse
    :: Int -- ^ 'ikmrsResponseStatus'
    -> ImportKeyMaterialResponse
importKeyMaterialResponse pResponseStatus_ =
  ImportKeyMaterialResponse' {_ikmrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ikmrsResponseStatus :: Lens' ImportKeyMaterialResponse Int
ikmrsResponseStatus = lens _ikmrsResponseStatus (\ s a -> s{_ikmrsResponseStatus = a});

instance NFData ImportKeyMaterialResponse where
