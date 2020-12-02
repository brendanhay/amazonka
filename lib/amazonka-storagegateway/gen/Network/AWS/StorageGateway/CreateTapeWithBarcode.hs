{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapeWithBarcode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual tape by using your own barcode. You write data to the virtual tape and then archive the tape. A barcode is unique and cannot be reused if it has already been used on a tape. This applies to barcodes used on deleted tapes. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CreateTapeWithBarcode
  ( -- * Creating a Request
    createTapeWithBarcode,
    CreateTapeWithBarcode,

    -- * Request Lenses
    ctwbKMSKey,
    ctwbKMSEncrypted,
    ctwbPoolId,
    ctwbWorm,
    ctwbTags,
    ctwbGatewayARN,
    ctwbTapeSizeInBytes,
    ctwbTapeBarcode,

    -- * Destructuring the Response
    createTapeWithBarcodeResponse,
    CreateTapeWithBarcodeResponse,

    -- * Response Lenses
    ctwbrsTapeARN,
    ctwbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | CreateTapeWithBarcodeInput
--
--
--
-- /See:/ 'createTapeWithBarcode' smart constructor.
data CreateTapeWithBarcode = CreateTapeWithBarcode'
  { _ctwbKMSKey ::
      !(Maybe Text),
    _ctwbKMSEncrypted :: !(Maybe Bool),
    _ctwbPoolId :: !(Maybe Text),
    _ctwbWorm :: !(Maybe Bool),
    _ctwbTags :: !(Maybe [Tag]),
    _ctwbGatewayARN :: !Text,
    _ctwbTapeSizeInBytes :: !Integer,
    _ctwbTapeBarcode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTapeWithBarcode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctwbKMSKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- * 'ctwbKMSEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
--
-- * 'ctwbPoolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- * 'ctwbWorm' - Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
--
-- * 'ctwbTags' - A list of up to 50 tags that can be assigned to a virtual tape that has a barcode. Each tag is a key-value pair.
--
-- * 'ctwbGatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- * 'ctwbTapeSizeInBytes' - The size, in bytes, of the virtual tape that you want to create.
--
-- * 'ctwbTapeBarcode' - The barcode that you want to assign to the tape.
createTapeWithBarcode ::
  -- | 'ctwbGatewayARN'
  Text ->
  -- | 'ctwbTapeSizeInBytes'
  Integer ->
  -- | 'ctwbTapeBarcode'
  Text ->
  CreateTapeWithBarcode
createTapeWithBarcode pGatewayARN_ pTapeSizeInBytes_ pTapeBarcode_ =
  CreateTapeWithBarcode'
    { _ctwbKMSKey = Nothing,
      _ctwbKMSEncrypted = Nothing,
      _ctwbPoolId = Nothing,
      _ctwbWorm = Nothing,
      _ctwbTags = Nothing,
      _ctwbGatewayARN = pGatewayARN_,
      _ctwbTapeSizeInBytes = pTapeSizeInBytes_,
      _ctwbTapeBarcode = pTapeBarcode_
    }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
ctwbKMSKey :: Lens' CreateTapeWithBarcode (Maybe Text)
ctwbKMSKey = lens _ctwbKMSKey (\s a -> s {_ctwbKMSKey = a})

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional. Valid Values: @true@ | @false@
ctwbKMSEncrypted :: Lens' CreateTapeWithBarcode (Maybe Bool)
ctwbKMSEncrypted = lens _ctwbKMSEncrypted (\s a -> s {_ctwbKMSEncrypted = a})

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
ctwbPoolId :: Lens' CreateTapeWithBarcode (Maybe Text)
ctwbPoolId = lens _ctwbPoolId (\s a -> s {_ctwbPoolId = a})

-- | Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
ctwbWorm :: Lens' CreateTapeWithBarcode (Maybe Bool)
ctwbWorm = lens _ctwbWorm (\s a -> s {_ctwbWorm = a})

-- | A list of up to 50 tags that can be assigned to a virtual tape that has a barcode. Each tag is a key-value pair.
ctwbTags :: Lens' CreateTapeWithBarcode [Tag]
ctwbTags = lens _ctwbTags (\s a -> s {_ctwbTags = a}) . _Default . _Coerce

-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
ctwbGatewayARN :: Lens' CreateTapeWithBarcode Text
ctwbGatewayARN = lens _ctwbGatewayARN (\s a -> s {_ctwbGatewayARN = a})

-- | The size, in bytes, of the virtual tape that you want to create.
ctwbTapeSizeInBytes :: Lens' CreateTapeWithBarcode Integer
ctwbTapeSizeInBytes = lens _ctwbTapeSizeInBytes (\s a -> s {_ctwbTapeSizeInBytes = a})

-- | The barcode that you want to assign to the tape.
ctwbTapeBarcode :: Lens' CreateTapeWithBarcode Text
ctwbTapeBarcode = lens _ctwbTapeBarcode (\s a -> s {_ctwbTapeBarcode = a})

instance AWSRequest CreateTapeWithBarcode where
  type Rs CreateTapeWithBarcode = CreateTapeWithBarcodeResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          CreateTapeWithBarcodeResponse'
            <$> (x .?> "TapeARN") <*> (pure (fromEnum s))
      )

instance Hashable CreateTapeWithBarcode

instance NFData CreateTapeWithBarcode

instance ToHeaders CreateTapeWithBarcode where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.CreateTapeWithBarcode" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTapeWithBarcode where
  toJSON CreateTapeWithBarcode' {..} =
    object
      ( catMaybes
          [ ("KMSKey" .=) <$> _ctwbKMSKey,
            ("KMSEncrypted" .=) <$> _ctwbKMSEncrypted,
            ("PoolId" .=) <$> _ctwbPoolId,
            ("Worm" .=) <$> _ctwbWorm,
            ("Tags" .=) <$> _ctwbTags,
            Just ("GatewayARN" .= _ctwbGatewayARN),
            Just ("TapeSizeInBytes" .= _ctwbTapeSizeInBytes),
            Just ("TapeBarcode" .= _ctwbTapeBarcode)
          ]
      )

instance ToPath CreateTapeWithBarcode where
  toPath = const "/"

instance ToQuery CreateTapeWithBarcode where
  toQuery = const mempty

-- | CreateTapeOutput
--
--
--
-- /See:/ 'createTapeWithBarcodeResponse' smart constructor.
data CreateTapeWithBarcodeResponse = CreateTapeWithBarcodeResponse'
  { _ctwbrsTapeARN ::
      !(Maybe Text),
    _ctwbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTapeWithBarcodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctwbrsTapeARN' - A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
--
-- * 'ctwbrsResponseStatus' - -- | The response status code.
createTapeWithBarcodeResponse ::
  -- | 'ctwbrsResponseStatus'
  Int ->
  CreateTapeWithBarcodeResponse
createTapeWithBarcodeResponse pResponseStatus_ =
  CreateTapeWithBarcodeResponse'
    { _ctwbrsTapeARN = Nothing,
      _ctwbrsResponseStatus = pResponseStatus_
    }

-- | A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
ctwbrsTapeARN :: Lens' CreateTapeWithBarcodeResponse (Maybe Text)
ctwbrsTapeARN = lens _ctwbrsTapeARN (\s a -> s {_ctwbrsTapeARN = a})

-- | -- | The response status code.
ctwbrsResponseStatus :: Lens' CreateTapeWithBarcodeResponse Int
ctwbrsResponseStatus = lens _ctwbrsResponseStatus (\s a -> s {_ctwbrsResponseStatus = a})

instance NFData CreateTapeWithBarcodeResponse
