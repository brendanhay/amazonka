{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.StopStreamEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables server-side encryption for a specified stream.
--
-- Stopping encryption is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Stopping encryption normally takes a few seconds to complete, but it can take minutes. You can continue to read and write data to your stream while its status is @UPDATING@ . Once the status of the stream is @ACTIVE@ , records written to the stream are no longer encrypted by Kinesis Data Streams.
-- API Limits: You can successfully disable server-side encryption 25 times in a rolling 24-hour period.
-- Note: It can take up to 5 seconds after the stream is in an @ACTIVE@ status before all records written to the stream are no longer subject to encryption. After you disabled encryption, you can verify that encryption is not applied by inspecting the API response from @PutRecord@ or @PutRecords@ .
module Network.AWS.Kinesis.StopStreamEncryption
  ( -- * Creating a request
    StopStreamEncryption (..),
    mkStopStreamEncryption,

    -- ** Request lenses
    sseEncryptionType,
    sseKeyId,
    sseStreamName,

    -- * Destructuring the response
    StopStreamEncryptionResponse (..),
    mkStopStreamEncryptionResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopStreamEncryption' smart constructor.
data StopStreamEncryption = StopStreamEncryption'
  { -- | The encryption type. The only valid value is @KMS@ .
    encryptionType :: EncryptionType,
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
    keyId :: Lude.Text,
    -- | The name of the stream on which to stop encrypting records.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopStreamEncryption' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The encryption type. The only valid value is @KMS@ .
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
-- * 'streamName' - The name of the stream on which to stop encrypting records.
mkStopStreamEncryption ::
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'keyId'
  Lude.Text ->
  -- | 'streamName'
  Lude.Text ->
  StopStreamEncryption
mkStopStreamEncryption pEncryptionType_ pKeyId_ pStreamName_ =
  StopStreamEncryption'
    { encryptionType = pEncryptionType_,
      keyId = pKeyId_,
      streamName = pStreamName_
    }

-- | The encryption type. The only valid value is @KMS@ .
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseEncryptionType :: Lens.Lens' StopStreamEncryption EncryptionType
sseEncryptionType = Lens.lens (encryptionType :: StopStreamEncryption -> EncryptionType) (\s a -> s {encryptionType = a} :: StopStreamEncryption)
{-# DEPRECATED sseEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

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
sseKeyId :: Lens.Lens' StopStreamEncryption Lude.Text
sseKeyId = Lens.lens (keyId :: StopStreamEncryption -> Lude.Text) (\s a -> s {keyId = a} :: StopStreamEncryption)
{-# DEPRECATED sseKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The name of the stream on which to stop encrypting records.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseStreamName :: Lens.Lens' StopStreamEncryption Lude.Text
sseStreamName = Lens.lens (streamName :: StopStreamEncryption -> Lude.Text) (\s a -> s {streamName = a} :: StopStreamEncryption)
{-# DEPRECATED sseStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest StopStreamEncryption where
  type Rs StopStreamEncryption = StopStreamEncryptionResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull StopStreamEncryptionResponse'

instance Lude.ToHeaders StopStreamEncryption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.StopStreamEncryption" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopStreamEncryption where
  toJSON StopStreamEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EncryptionType" Lude..= encryptionType),
            Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath StopStreamEncryption where
  toPath = Lude.const "/"

instance Lude.ToQuery StopStreamEncryption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopStreamEncryptionResponse' smart constructor.
data StopStreamEncryptionResponse = StopStreamEncryptionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopStreamEncryptionResponse' with the minimum fields required to make a request.
mkStopStreamEncryptionResponse ::
  StopStreamEncryptionResponse
mkStopStreamEncryptionResponse = StopStreamEncryptionResponse'
