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
    sseStreamName,
    sseEncryptionType,
    sseKeyId,

    -- * Destructuring the response
    StopStreamEncryptionResponse (..),
    mkStopStreamEncryptionResponse,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopStreamEncryption' smart constructor.
data StopStreamEncryption = StopStreamEncryption'
  { -- | The name of the stream on which to stop encrypting records.
    streamName :: Types.StreamName,
    -- | The encryption type. The only valid value is @KMS@ .
    encryptionType :: Types.EncryptionType,
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
    keyId :: Types.KeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopStreamEncryption' value with any optional fields omitted.
mkStopStreamEncryption ::
  -- | 'streamName'
  Types.StreamName ->
  -- | 'encryptionType'
  Types.EncryptionType ->
  -- | 'keyId'
  Types.KeyId ->
  StopStreamEncryption
mkStopStreamEncryption streamName encryptionType keyId =
  StopStreamEncryption' {streamName, encryptionType, keyId}

-- | The name of the stream on which to stop encrypting records.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseStreamName :: Lens.Lens' StopStreamEncryption Types.StreamName
sseStreamName = Lens.field @"streamName"
{-# DEPRECATED sseStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The encryption type. The only valid value is @KMS@ .
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseEncryptionType :: Lens.Lens' StopStreamEncryption Types.EncryptionType
sseEncryptionType = Lens.field @"encryptionType"
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
sseKeyId :: Lens.Lens' StopStreamEncryption Types.KeyId
sseKeyId = Lens.field @"keyId"
{-# DEPRECATED sseKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Core.FromJSON StopStreamEncryption where
  toJSON StopStreamEncryption {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("EncryptionType" Core..= encryptionType),
            Core.Just ("KeyId" Core..= keyId)
          ]
      )

instance Core.AWSRequest StopStreamEncryption where
  type Rs StopStreamEncryption = StopStreamEncryptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.StopStreamEncryption")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopStreamEncryptionResponse'

-- | /See:/ 'mkStopStreamEncryptionResponse' smart constructor.
data StopStreamEncryptionResponse = StopStreamEncryptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopStreamEncryptionResponse' value with any optional fields omitted.
mkStopStreamEncryptionResponse ::
  StopStreamEncryptionResponse
mkStopStreamEncryptionResponse = StopStreamEncryptionResponse'
