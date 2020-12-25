{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CreateLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a log group with the specified name. You can create up to 20,000 log groups per account.
--
-- You must use the following guidelines when naming a log group:
--
--     * Log group names must be unique within a region for an AWS account.
--
--
--     * Log group names can be between 1 and 512 characters long.
--
--
--     * Log group names consist of the following characters: a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), '.' (period), and '#' (number sign)
--
--
-- When you create a log group, by default the log events in the log group never expire. To set a retention policy so that events expire and are deleted after a specified time, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutRetentionPolicy.html PutRetentionPolicy> .
-- If you associate a AWS Key Management Service (AWS KMS) customer master key (CMK) with the log group, ingested data is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.
-- If you attempt to associate a CMK with the log group but the CMK does not exist or the CMK is disabled, you receive an @InvalidParameterException@ error.
-- /Important:/ CloudWatch Logs supports only symmetric CMKs. Do not associate an asymmetric CMK with your log group. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
module Network.AWS.CloudWatchLogs.CreateLogGroup
  ( -- * Creating a request
    CreateLogGroup (..),
    mkCreateLogGroup,

    -- ** Request lenses
    clgLogGroupName,
    clgKmsKeyId,
    clgTags,

    -- * Destructuring the response
    CreateLogGroupResponse (..),
    mkCreateLogGroupResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLogGroup' smart constructor.
data CreateLogGroup = CreateLogGroup'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The key-value pairs to use for the tags.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogGroup' value with any optional fields omitted.
mkCreateLogGroup ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  CreateLogGroup
mkCreateLogGroup logGroupName =
  CreateLogGroup'
    { logGroupName,
      kmsKeyId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgLogGroupName :: Lens.Lens' CreateLogGroup Types.LogGroupName
clgLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED clgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgKmsKeyId :: Lens.Lens' CreateLogGroup (Core.Maybe Types.KmsKeyId)
clgKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED clgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The key-value pairs to use for the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgTags :: Lens.Lens' CreateLogGroup (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
clgTags = Lens.field @"tags"
{-# DEPRECATED clgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateLogGroup where
  toJSON CreateLogGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateLogGroup where
  type Rs CreateLogGroup = CreateLogGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.CreateLogGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CreateLogGroupResponse'

-- | /See:/ 'mkCreateLogGroupResponse' smart constructor.
data CreateLogGroupResponse = CreateLogGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogGroupResponse' value with any optional fields omitted.
mkCreateLogGroupResponse ::
  CreateLogGroupResponse
mkCreateLogGroupResponse = CreateLogGroupResponse'
