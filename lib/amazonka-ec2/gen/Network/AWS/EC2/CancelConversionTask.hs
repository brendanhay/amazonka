{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelConversionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an active conversion task. The task can be the import of an instance or volume. The action removes all artifacts of the conversion, including a partially uploaded volume or instance. If the conversion is complete or is in the process of transferring the final disk image, the command fails and returns an exception.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI> .
module Network.AWS.EC2.CancelConversionTask
  ( -- * Creating a request
    CancelConversionTask (..),
    mkCancelConversionTask,

    -- ** Request lenses
    cctReasonMessage,
    cctConversionTaskId,
    cctDryRun,

    -- * Destructuring the response
    CancelConversionTaskResponse (..),
    mkCancelConversionTaskResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelConversionTask' smart constructor.
data CancelConversionTask = CancelConversionTask'
  { -- | The reason for canceling the conversion task.
    reasonMessage :: Lude.Maybe Lude.Text,
    -- | The ID of the conversion task.
    conversionTaskId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelConversionTask' with the minimum fields required to make a request.
--
-- * 'reasonMessage' - The reason for canceling the conversion task.
-- * 'conversionTaskId' - The ID of the conversion task.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCancelConversionTask ::
  -- | 'conversionTaskId'
  Lude.Text ->
  CancelConversionTask
mkCancelConversionTask pConversionTaskId_ =
  CancelConversionTask'
    { reasonMessage = Lude.Nothing,
      conversionTaskId = pConversionTaskId_,
      dryRun = Lude.Nothing
    }

-- | The reason for canceling the conversion task.
--
-- /Note:/ Consider using 'reasonMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctReasonMessage :: Lens.Lens' CancelConversionTask (Lude.Maybe Lude.Text)
cctReasonMessage = Lens.lens (reasonMessage :: CancelConversionTask -> Lude.Maybe Lude.Text) (\s a -> s {reasonMessage = a} :: CancelConversionTask)
{-# DEPRECATED cctReasonMessage "Use generic-lens or generic-optics with 'reasonMessage' instead." #-}

-- | The ID of the conversion task.
--
-- /Note:/ Consider using 'conversionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctConversionTaskId :: Lens.Lens' CancelConversionTask Lude.Text
cctConversionTaskId = Lens.lens (conversionTaskId :: CancelConversionTask -> Lude.Text) (\s a -> s {conversionTaskId = a} :: CancelConversionTask)
{-# DEPRECATED cctConversionTaskId "Use generic-lens or generic-optics with 'conversionTaskId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctDryRun :: Lens.Lens' CancelConversionTask (Lude.Maybe Lude.Bool)
cctDryRun = Lens.lens (dryRun :: CancelConversionTask -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CancelConversionTask)
{-# DEPRECATED cctDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CancelConversionTask where
  type Rs CancelConversionTask = CancelConversionTaskResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull CancelConversionTaskResponse'

instance Lude.ToHeaders CancelConversionTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelConversionTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelConversionTask where
  toQuery CancelConversionTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelConversionTask" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ReasonMessage" Lude.=: reasonMessage,
        "ConversionTaskId" Lude.=: conversionTaskId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCancelConversionTaskResponse' smart constructor.
data CancelConversionTaskResponse = CancelConversionTaskResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelConversionTaskResponse' with the minimum fields required to make a request.
mkCancelConversionTaskResponse ::
  CancelConversionTaskResponse
mkCancelConversionTaskResponse = CancelConversionTaskResponse'
