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
    rhpResourceModel,
    rhpBearerToken,
    rhpOperationStatus,
    rhpStatusMessage,
    rhpErrorCode,
    rhpCurrentOperationStatus,
    rhpClientRequestToken,

    -- * Destructuring the response
    RecordHandlerProgressResponse (..),
    mkRecordHandlerProgressResponse,

    -- ** Response lenses
    rhprsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRecordHandlerProgress' smart constructor.
data RecordHandlerProgress = RecordHandlerProgress'
  { -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    resourceModel :: Lude.Maybe Lude.Text,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    bearerToken :: Lude.Text,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    operationStatus :: OperationStatus,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    statusMessage :: Lude.Maybe Lude.Text,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    errorCode :: Lude.Maybe HandlerErrorCode,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    currentOperationStatus :: Lude.Maybe OperationStatus,
    -- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordHandlerProgress' with the minimum fields required to make a request.
--
-- * 'resourceModel' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
-- * 'bearerToken' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
-- * 'operationStatus' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
-- * 'statusMessage' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
-- * 'errorCode' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
-- * 'currentOperationStatus' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
-- * 'clientRequestToken' - Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
mkRecordHandlerProgress ::
  -- | 'bearerToken'
  Lude.Text ->
  -- | 'operationStatus'
  OperationStatus ->
  RecordHandlerProgress
mkRecordHandlerProgress pBearerToken_ pOperationStatus_ =
  RecordHandlerProgress'
    { resourceModel = Lude.Nothing,
      bearerToken = pBearerToken_,
      operationStatus = pOperationStatus_,
      statusMessage = Lude.Nothing,
      errorCode = Lude.Nothing,
      currentOperationStatus = Lude.Nothing,
      clientRequestToken = Lude.Nothing
    }

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'resourceModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpResourceModel :: Lens.Lens' RecordHandlerProgress (Lude.Maybe Lude.Text)
rhpResourceModel = Lens.lens (resourceModel :: RecordHandlerProgress -> Lude.Maybe Lude.Text) (\s a -> s {resourceModel = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpResourceModel "Use generic-lens or generic-optics with 'resourceModel' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'bearerToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpBearerToken :: Lens.Lens' RecordHandlerProgress Lude.Text
rhpBearerToken = Lens.lens (bearerToken :: RecordHandlerProgress -> Lude.Text) (\s a -> s {bearerToken = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpBearerToken "Use generic-lens or generic-optics with 'bearerToken' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'operationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpOperationStatus :: Lens.Lens' RecordHandlerProgress OperationStatus
rhpOperationStatus = Lens.lens (operationStatus :: RecordHandlerProgress -> OperationStatus) (\s a -> s {operationStatus = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpOperationStatus "Use generic-lens or generic-optics with 'operationStatus' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpStatusMessage :: Lens.Lens' RecordHandlerProgress (Lude.Maybe Lude.Text)
rhpStatusMessage = Lens.lens (statusMessage :: RecordHandlerProgress -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpErrorCode :: Lens.Lens' RecordHandlerProgress (Lude.Maybe HandlerErrorCode)
rhpErrorCode = Lens.lens (errorCode :: RecordHandlerProgress -> Lude.Maybe HandlerErrorCode) (\s a -> s {errorCode = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'currentOperationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpCurrentOperationStatus :: Lens.Lens' RecordHandlerProgress (Lude.Maybe OperationStatus)
rhpCurrentOperationStatus = Lens.lens (currentOperationStatus :: RecordHandlerProgress -> Lude.Maybe OperationStatus) (\s a -> s {currentOperationStatus = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpCurrentOperationStatus "Use generic-lens or generic-optics with 'currentOperationStatus' instead." #-}

-- | Reserved for use by the <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/what-is-cloudformation-cli.html CloudFormation CLI> .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhpClientRequestToken :: Lens.Lens' RecordHandlerProgress (Lude.Maybe Lude.Text)
rhpClientRequestToken = Lens.lens (clientRequestToken :: RecordHandlerProgress -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: RecordHandlerProgress)
{-# DEPRECATED rhpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest RecordHandlerProgress where
  type Rs RecordHandlerProgress = RecordHandlerProgressResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "RecordHandlerProgressResult"
      ( \s h x ->
          RecordHandlerProgressResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RecordHandlerProgress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RecordHandlerProgress where
  toPath = Lude.const "/"

instance Lude.ToQuery RecordHandlerProgress where
  toQuery RecordHandlerProgress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RecordHandlerProgress" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ResourceModel" Lude.=: resourceModel,
        "BearerToken" Lude.=: bearerToken,
        "OperationStatus" Lude.=: operationStatus,
        "StatusMessage" Lude.=: statusMessage,
        "ErrorCode" Lude.=: errorCode,
        "CurrentOperationStatus" Lude.=: currentOperationStatus,
        "ClientRequestToken" Lude.=: clientRequestToken
      ]

-- | /See:/ 'mkRecordHandlerProgressResponse' smart constructor.
newtype RecordHandlerProgressResponse = RecordHandlerProgressResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordHandlerProgressResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRecordHandlerProgressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RecordHandlerProgressResponse
mkRecordHandlerProgressResponse pResponseStatus_ =
  RecordHandlerProgressResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhprsResponseStatus :: Lens.Lens' RecordHandlerProgressResponse Lude.Int
rhprsResponseStatus = Lens.lens (responseStatus :: RecordHandlerProgressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RecordHandlerProgressResponse)
{-# DEPRECATED rhprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
