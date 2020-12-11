{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterDefaultPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the default patch baseline for the relevant operating system.
--
-- To reset the AWS predefined patch baseline as the default, specify the full patch baseline ARN as the baseline ID value. For example, for CentOS, specify @arn:aws:ssm:us-east-2:733109147000:patchbaseline/pb-0574b43a65ea646ed@ instead of @pb-0574b43a65ea646ed@ .
module Network.AWS.SSM.RegisterDefaultPatchBaseline
  ( -- * Creating a request
    RegisterDefaultPatchBaseline (..),
    mkRegisterDefaultPatchBaseline,

    -- ** Request lenses
    rdpbBaselineId,

    -- * Destructuring the response
    RegisterDefaultPatchBaselineResponse (..),
    mkRegisterDefaultPatchBaselineResponse,

    -- ** Response lenses
    rdpbrsBaselineId,
    rdpbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkRegisterDefaultPatchBaseline' smart constructor.
newtype RegisterDefaultPatchBaseline = RegisterDefaultPatchBaseline'
  { baselineId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDefaultPatchBaseline' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline that should be the default patch baseline.
mkRegisterDefaultPatchBaseline ::
  -- | 'baselineId'
  Lude.Text ->
  RegisterDefaultPatchBaseline
mkRegisterDefaultPatchBaseline pBaselineId_ =
  RegisterDefaultPatchBaseline' {baselineId = pBaselineId_}

-- | The ID of the patch baseline that should be the default patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpbBaselineId :: Lens.Lens' RegisterDefaultPatchBaseline Lude.Text
rdpbBaselineId = Lens.lens (baselineId :: RegisterDefaultPatchBaseline -> Lude.Text) (\s a -> s {baselineId = a} :: RegisterDefaultPatchBaseline)
{-# DEPRECATED rdpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Lude.AWSRequest RegisterDefaultPatchBaseline where
  type
    Rs RegisterDefaultPatchBaseline =
      RegisterDefaultPatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterDefaultPatchBaselineResponse'
            Lude.<$> (x Lude..?> "BaselineId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterDefaultPatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.RegisterDefaultPatchBaseline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterDefaultPatchBaseline where
  toJSON RegisterDefaultPatchBaseline' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BaselineId" Lude..= baselineId)])

instance Lude.ToPath RegisterDefaultPatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterDefaultPatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterDefaultPatchBaselineResponse' smart constructor.
data RegisterDefaultPatchBaselineResponse = RegisterDefaultPatchBaselineResponse'
  { baselineId ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'RegisterDefaultPatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the default patch baseline.
-- * 'responseStatus' - The response status code.
mkRegisterDefaultPatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterDefaultPatchBaselineResponse
mkRegisterDefaultPatchBaselineResponse pResponseStatus_ =
  RegisterDefaultPatchBaselineResponse'
    { baselineId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the default patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpbrsBaselineId :: Lens.Lens' RegisterDefaultPatchBaselineResponse (Lude.Maybe Lude.Text)
rdpbrsBaselineId = Lens.lens (baselineId :: RegisterDefaultPatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: RegisterDefaultPatchBaselineResponse)
{-# DEPRECATED rdpbrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpbrsResponseStatus :: Lens.Lens' RegisterDefaultPatchBaselineResponse Lude.Int
rdpbrsResponseStatus = Lens.lens (responseStatus :: RegisterDefaultPatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterDefaultPatchBaselineResponse)
{-# DEPRECATED rdpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
