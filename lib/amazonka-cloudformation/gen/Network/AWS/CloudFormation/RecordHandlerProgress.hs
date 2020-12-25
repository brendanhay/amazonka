{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.RecordHandlerProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reports progress of a resource handler to CloudFormation.
--
-- Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> . Do not use this API in your code.
module Network.AWS.CloudFormation.RecordHandlerProgress
  ( -- * Creating a request
    RecordHandlerProgress (..),
    mkRecordHandlerProgress,

    -- ** Request lenses
    rhpBearerToken,
    rhpOperationStatus,
    rhpClientRequestToken,
    rhpCurrentOperationStatus,
    rhpErrorCode,
    rhpResourceModel,
    rhpStatusMessage,

    -- * Destructuring the response
    RecordHandlerProgressResponse (..),
    mkRecordHandlerProgressResponse,

    -- ** Response lenses
    rhprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRecordHandlerProgress' smart constructor.
data RecordHandlerProgress = RecordHandlerProgress'
  { -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    bearerToken :: Types.ClientToken,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    operationStatus :: Types.OperationStatus,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    currentOperationStatus :: Core.Maybe Types.OperationStatus,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    errorCode :: Core.Maybe Types.HandlerErrorCode,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    resourceModel :: Core.Maybe Types.ResourceModel,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    statusMessage :: Core.Maybe Types.StatusMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordHandlerProgress' value with any optional fields omitted.
mkRecordHandlerProgress ::
  -- | 'bearerToken'
  Types.ClientToken ->
  -- | 'operationStatus'
  Types.OperationStatus ->
  RecordHandlerProgress
mkRecordHandlerProgress bearerToken operationStatus =
  RecordHandlerProgress'
    { bearerToken,
      operationStatus,
      clientRequestToken = Core.Nothing,
      currentOperationStatus = Core.Nothing,
      errorCode = Core.Nothing,
      resourceModel = Core.Nothing,
      statusMessage = Core.Nothing
    }

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'bearerToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpBearerToken :: Lens.Lens' RecordHandlerProgress Types.ClientToken
rhpBearerToken = Lens.field @"bearerToken"
{-# DEPRECATED rhpBearerToken "Use generic-lens or generic-optics with 'bearerToken' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'operationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpOperationStatus :: Lens.Lens' RecordHandlerProgress Types.OperationStatus
rhpOperationStatus = Lens.field @"operationStatus"
{-# DEPRECATED rhpOperationStatus "Use generic-lens or generic-optics with 'operationStatus' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpClientRequestToken :: Lens.Lens' RecordHandlerProgress (Core.Maybe Types.ClientRequestToken)
rhpClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED rhpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'currentOperationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpCurrentOperationStatus :: Lens.Lens' RecordHandlerProgress (Core.Maybe Types.OperationStatus)
rhpCurrentOperationStatus = Lens.field @"currentOperationStatus"
{-# DEPRECATED rhpCurrentOperationStatus "Use generic-lens or generic-optics with 'currentOperationStatus' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpErrorCode :: Lens.Lens' RecordHandlerProgress (Core.Maybe Types.HandlerErrorCode)
rhpErrorCode = Lens.field @"errorCode"
{-# DEPRECATED rhpErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'resourceModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpResourceModel :: Lens.Lens' RecordHandlerProgress (Core.Maybe Types.ResourceModel)
rhpResourceModel = Lens.field @"resourceModel"
{-# DEPRECATED rhpResourceModel "Use generic-lens or generic-optics with 'resourceModel' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpStatusMessage :: Lens.Lens' RecordHandlerProgress (Core.Maybe Types.StatusMessage)
rhpStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED rhpStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Core.AWSRequest RecordHandlerProgress where
  type Rs RecordHandlerProgress = RecordHandlerProgressResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RecordHandlerProgress")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "BearerToken" bearerToken)
                Core.<> (Core.toQueryValue "OperationStatus" operationStatus)
                Core.<> ( Core.toQueryValue "ClientRequestToken"
                            Core.<$> clientRequestToken
                        )
                Core.<> ( Core.toQueryValue "CurrentOperationStatus"
                            Core.<$> currentOperationStatus
                        )
                Core.<> (Core.toQueryValue "ErrorCode" Core.<$> errorCode)
                Core.<> (Core.toQueryValue "ResourceModel" Core.<$> resourceModel)
                Core.<> (Core.toQueryValue "StatusMessage" Core.<$> statusMessage)
            )
      }
  response =
    Response.receiveXMLWrapper
      "RecordHandlerProgressResult"
      ( \s h x ->
          RecordHandlerProgressResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRecordHandlerProgressResponse' smart constructor.
newtype RecordHandlerProgressResponse = RecordHandlerProgressResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RecordHandlerProgressResponse' value with any optional fields omitted.
mkRecordHandlerProgressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RecordHandlerProgressResponse
mkRecordHandlerProgressResponse responseStatus =
  RecordHandlerProgressResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhprrsResponseStatus :: Lens.Lens' RecordHandlerProgressResponse Core.Int
rhprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rhprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
