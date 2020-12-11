{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.StartStreamEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or updates server-side encryption using an AWS KMS key for a specified stream.
--
-- Starting encryption is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Updating or applying encryption normally takes a few seconds to complete, but it can take minutes. You can continue to read and write data to your stream while its status is @UPDATING@ . Once the status of the stream is @ACTIVE@ , encryption begins for records written to the stream.
-- API Limits: You can successfully apply a new AWS KMS key for server-side encryption 25 times in a rolling 24-hour period.
-- Note: It can take up to 5 seconds after the stream is in an @ACTIVE@ status before all records written to the stream are encrypted. After you enable encryption, you can verify that encryption is applied by inspecting the API response from @PutRecord@ or @PutRecords@ .
module Network.AWS.Kinesis.StartStreamEncryption
  ( -- * Creating a request
    StartStreamEncryption (..),
    mkStartStreamEncryption,

    -- ** Request lenses
    sStreamName,
    sEncryptionType,
    sKeyId,

    -- * Destructuring the response
    StartStreamEncryptionResponse (..),
    mkStartStreamEncryptionResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartStreamEncryption' smart constructor.
data StartStreamEncryption = StartStreamEncryption'
  { streamName ::
      Lude.Text,
    encryptionType :: EncryptionType,
    keyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartStreamEncryption' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The encryption type to use. The only valid value is @KMS@ .
-- * 'keyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified Amazon Resource Name (ARN) to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .
--
--
--     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
--
--
--     * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@
--
--
--     * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@
--
--
--     * Alias name example: @alias/MyAliasName@
--
--
--     * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
--
--
-- * 'streamName' - The name of the stream for which to start encrypting records.
mkStartStreamEncryption ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'keyId'
  Lude.Text ->
  StartStreamEncryption
mkStartStreamEncryption pStreamName_ pEncryptionType_ pKeyId_ =
  StartStreamEncryption'
    { streamName = pStreamName_,
      encryptionType = pEncryptionType_,
      keyId = pKeyId_
    }

-- | The name of the stream for which to start encrypting records.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStreamName :: Lens.Lens' StartStreamEncryption Lude.Text
sStreamName = Lens.lens (streamName :: StartStreamEncryption -> Lude.Text) (\s a -> s {streamName = a} :: StartStreamEncryption)
{-# DEPRECATED sStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The encryption type to use. The only valid value is @KMS@ .
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptionType :: Lens.Lens' StartStreamEncryption EncryptionType
sEncryptionType = Lens.lens (encryptionType :: StartStreamEncryption -> EncryptionType) (\s a -> s {encryptionType = a} :: StartStreamEncryption)
{-# DEPRECATED sEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified Amazon Resource Name (ARN) to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .
--
--
--     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
--
--
--     * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@
--
--
--     * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@
--
--
--     * Alias name example: @alias/MyAliasName@
--
--
--     * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
--
--
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKeyId :: Lens.Lens' StartStreamEncryption Lude.Text
sKeyId = Lens.lens (keyId :: StartStreamEncryption -> Lude.Text) (\s a -> s {keyId = a} :: StartStreamEncryption)
{-# DEPRECATED sKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest StartStreamEncryption where
  type Rs StartStreamEncryption = StartStreamEncryptionResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull StartStreamEncryptionResponse'

instance Lude.ToHeaders StartStreamEncryption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.StartStreamEncryption" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartStreamEncryption where
  toJSON StartStreamEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamName" Lude..= streamName),
            Lude.Just ("EncryptionType" Lude..= encryptionType),
            Lude.Just ("KeyId" Lude..= keyId)
          ]
      )

instance Lude.ToPath StartStreamEncryption where
  toPath = Lude.const "/"

instance Lude.ToQuery StartStreamEncryption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartStreamEncryptionResponse' smart constructor.
data StartStreamEncryptionResponse = StartStreamEncryptionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartStreamEncryptionResponse' with the minimum fields required to make a request.
mkStartStreamEncryptionResponse ::
  StartStreamEncryptionResponse
mkStartStreamEncryptionResponse = StartStreamEncryptionResponse'
