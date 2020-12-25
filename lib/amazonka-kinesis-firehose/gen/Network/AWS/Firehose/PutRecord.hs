{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.PutRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis Data Firehose delivery stream. To write multiple data records into a delivery stream, use 'PutRecordBatch' . Applications using these operations are referred to as producers.
--
-- By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use 'PutRecord' and 'PutRecordBatch' , the limits are an aggregate across these two operations for each delivery stream. For more information about limits and how to request an increase, see <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits> .
-- You must specify the name of the delivery stream and the data record when using 'PutRecord' . The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it can be a segment from a log file, geographic location data, website clickstream data, and so on.
-- Kinesis Data Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (@\n@ ) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.
-- The @PutRecord@ operation returns a @RecordId@ , which is a unique string assigned to each record. Producer applications can use this ID for purposes such as auditability and investigation.
-- If the @PutRecord@ operation throws a @ServiceUnavailableException@ , back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.
-- Data records sent to Kinesis Data Firehose are stored for 24 hours from the time they are added to a delivery stream as it tries to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.
-- /Important:/ Don't concatenate two or more base64 strings to form the data fields of your records. Instead, concatenate the raw data, then perform base64 encoding.
module Network.AWS.Firehose.PutRecord
  ( -- * Creating a request
    PutRecord (..),
    mkPutRecord,

    -- ** Request lenses
    prDeliveryStreamName,
    prRecord,

    -- * Destructuring the response
    PutRecordResponse (..),
    mkPutRecordResponse,

    -- ** Response lenses
    prrrsRecordId,
    prrrsEncrypted,
    prrrsResponseStatus,
  )
where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRecord' smart constructor.
data PutRecord = PutRecord'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Types.DeliveryStreamName,
    -- | The record.
    record :: Types.Record
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecord' value with any optional fields omitted.
mkPutRecord ::
  -- | 'deliveryStreamName'
  Types.DeliveryStreamName ->
  -- | 'record'
  Types.Record ->
  PutRecord
mkPutRecord deliveryStreamName record =
  PutRecord' {deliveryStreamName, record}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prDeliveryStreamName :: Lens.Lens' PutRecord Types.DeliveryStreamName
prDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# DEPRECATED prDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | The record.
--
-- /Note:/ Consider using 'record' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRecord :: Lens.Lens' PutRecord Types.Record
prRecord = Lens.field @"record"
{-# DEPRECATED prRecord "Use generic-lens or generic-optics with 'record' instead." #-}

instance Core.FromJSON PutRecord where
  toJSON PutRecord {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
            Core.Just ("Record" Core..= record)
          ]
      )

instance Core.AWSRequest PutRecord where
  type Rs PutRecord = PutRecordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Firehose_20150804.PutRecord")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRecordResponse'
            Core.<$> (x Core..: "RecordId")
            Core.<*> (x Core..:? "Encrypted")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { -- | The ID of the record.
    recordId :: Types.PutResponseRecordId,
    -- | Indicates whether server-side encryption (SSE) was enabled during this operation.
    encrypted :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordResponse' value with any optional fields omitted.
mkPutRecordResponse ::
  -- | 'recordId'
  Types.PutResponseRecordId ->
  -- | 'responseStatus'
  Core.Int ->
  PutRecordResponse
mkPutRecordResponse recordId responseStatus =
  PutRecordResponse'
    { recordId,
      encrypted = Core.Nothing,
      responseStatus
    }

-- | The ID of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsRecordId :: Lens.Lens' PutRecordResponse Types.PutResponseRecordId
prrrsRecordId = Lens.field @"recordId"
{-# DEPRECATED prrrsRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}

-- | Indicates whether server-side encryption (SSE) was enabled during this operation.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsEncrypted :: Lens.Lens' PutRecordResponse (Core.Maybe Core.Bool)
prrrsEncrypted = Lens.field @"encrypted"
{-# DEPRECATED prrrsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrrsResponseStatus :: Lens.Lens' PutRecordResponse Core.Int
prrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
