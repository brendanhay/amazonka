{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a shipping label that will be used to return the Snow device to AWS.
module Network.AWS.Snowball.CreateReturnShippingLabel
  ( -- * Creating a request
    CreateReturnShippingLabel (..),
    mkCreateReturnShippingLabel,

    -- ** Request lenses
    crslShippingOption,
    crslJobId,

    -- * Destructuring the response
    CreateReturnShippingLabelResponse (..),
    mkCreateReturnShippingLabelResponse,

    -- ** Response lenses
    crslrsStatus,
    crslrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkCreateReturnShippingLabel' smart constructor.
data CreateReturnShippingLabel = CreateReturnShippingLabel'
  { shippingOption ::
      Lude.Maybe ShippingOption,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReturnShippingLabel' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID for a job that you want to create the return shipping label for. For example @JID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'shippingOption' - The shipping speed for a particular job. This speed doesn't dictate how soon the device is returned to AWS. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
mkCreateReturnShippingLabel ::
  -- | 'jobId'
  Lude.Text ->
  CreateReturnShippingLabel
mkCreateReturnShippingLabel pJobId_ =
  CreateReturnShippingLabel'
    { shippingOption = Lude.Nothing,
      jobId = pJobId_
    }

-- | The shipping speed for a particular job. This speed doesn't dictate how soon the device is returned to AWS. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslShippingOption :: Lens.Lens' CreateReturnShippingLabel (Lude.Maybe ShippingOption)
crslShippingOption = Lens.lens (shippingOption :: CreateReturnShippingLabel -> Lude.Maybe ShippingOption) (\s a -> s {shippingOption = a} :: CreateReturnShippingLabel)
{-# DEPRECATED crslShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The ID for a job that you want to create the return shipping label for. For example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslJobId :: Lens.Lens' CreateReturnShippingLabel Lude.Text
crslJobId = Lens.lens (jobId :: CreateReturnShippingLabel -> Lude.Text) (\s a -> s {jobId = a} :: CreateReturnShippingLabel)
{-# DEPRECATED crslJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest CreateReturnShippingLabel where
  type
    Rs CreateReturnShippingLabel =
      CreateReturnShippingLabelResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateReturnShippingLabelResponse'
            Lude.<$> (x Lude..?> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReturnShippingLabel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.CreateReturnShippingLabel" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateReturnShippingLabel where
  toJSON CreateReturnShippingLabel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ShippingOption" Lude..=) Lude.<$> shippingOption,
            Lude.Just ("JobId" Lude..= jobId)
          ]
      )

instance Lude.ToPath CreateReturnShippingLabel where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReturnShippingLabel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateReturnShippingLabelResponse' smart constructor.
data CreateReturnShippingLabelResponse = CreateReturnShippingLabelResponse'
  { status ::
      Lude.Maybe
        ShippingLabelStatus,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReturnShippingLabelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The status information of the task on a Snow device that is being returned to AWS.
mkCreateReturnShippingLabelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReturnShippingLabelResponse
mkCreateReturnShippingLabelResponse pResponseStatus_ =
  CreateReturnShippingLabelResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status information of the task on a Snow device that is being returned to AWS.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslrsStatus :: Lens.Lens' CreateReturnShippingLabelResponse (Lude.Maybe ShippingLabelStatus)
crslrsStatus = Lens.lens (status :: CreateReturnShippingLabelResponse -> Lude.Maybe ShippingLabelStatus) (\s a -> s {status = a} :: CreateReturnShippingLabelResponse)
{-# DEPRECATED crslrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslrsResponseStatus :: Lens.Lens' CreateReturnShippingLabelResponse Lude.Int
crslrsResponseStatus = Lens.lens (responseStatus :: CreateReturnShippingLabelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReturnShippingLabelResponse)
{-# DEPRECATED crslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
