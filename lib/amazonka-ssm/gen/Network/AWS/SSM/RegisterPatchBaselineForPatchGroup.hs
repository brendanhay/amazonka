{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a patch baseline for a patch group.
module Network.AWS.SSM.RegisterPatchBaselineForPatchGroup
  ( -- * Creating a request
    RegisterPatchBaselineForPatchGroup (..),
    mkRegisterPatchBaselineForPatchGroup,

    -- ** Request lenses
    rpbfpgBaselineId,
    rpbfpgPatchGroup,

    -- * Destructuring the response
    RegisterPatchBaselineForPatchGroupResponse (..),
    mkRegisterPatchBaselineForPatchGroupResponse,

    -- ** Response lenses
    rpbfpgrsBaselineId,
    rpbfpgrsPatchGroup,
    rpbfpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkRegisterPatchBaselineForPatchGroup' smart constructor.
data RegisterPatchBaselineForPatchGroup = RegisterPatchBaselineForPatchGroup'
  { baselineId ::
      Lude.Text,
    patchGroup ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterPatchBaselineForPatchGroup' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline to register the patch group with.
-- * 'patchGroup' - The name of the patch group that should be registered with the patch baseline.
mkRegisterPatchBaselineForPatchGroup ::
  -- | 'baselineId'
  Lude.Text ->
  -- | 'patchGroup'
  Lude.Text ->
  RegisterPatchBaselineForPatchGroup
mkRegisterPatchBaselineForPatchGroup pBaselineId_ pPatchGroup_ =
  RegisterPatchBaselineForPatchGroup'
    { baselineId = pBaselineId_,
      patchGroup = pPatchGroup_
    }

-- | The ID of the patch baseline to register the patch group with.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgBaselineId :: Lens.Lens' RegisterPatchBaselineForPatchGroup Lude.Text
rpbfpgBaselineId = Lens.lens (baselineId :: RegisterPatchBaselineForPatchGroup -> Lude.Text) (\s a -> s {baselineId = a} :: RegisterPatchBaselineForPatchGroup)
{-# DEPRECATED rpbfpgBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group that should be registered with the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgPatchGroup :: Lens.Lens' RegisterPatchBaselineForPatchGroup Lude.Text
rpbfpgPatchGroup = Lens.lens (patchGroup :: RegisterPatchBaselineForPatchGroup -> Lude.Text) (\s a -> s {patchGroup = a} :: RegisterPatchBaselineForPatchGroup)
{-# DEPRECATED rpbfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Lude.AWSRequest RegisterPatchBaselineForPatchGroup where
  type
    Rs RegisterPatchBaselineForPatchGroup =
      RegisterPatchBaselineForPatchGroupResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterPatchBaselineForPatchGroupResponse'
            Lude.<$> (x Lude..?> "BaselineId")
            Lude.<*> (x Lude..?> "PatchGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterPatchBaselineForPatchGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.RegisterPatchBaselineForPatchGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterPatchBaselineForPatchGroup where
  toJSON RegisterPatchBaselineForPatchGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BaselineId" Lude..= baselineId),
            Lude.Just ("PatchGroup" Lude..= patchGroup)
          ]
      )

instance Lude.ToPath RegisterPatchBaselineForPatchGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterPatchBaselineForPatchGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterPatchBaselineForPatchGroupResponse' smart constructor.
data RegisterPatchBaselineForPatchGroupResponse = RegisterPatchBaselineForPatchGroupResponse'
  { baselineId ::
      Lude.Maybe
        Lude.Text,
    patchGroup ::
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

-- | Creates a value of 'RegisterPatchBaselineForPatchGroupResponse' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline the patch group was registered with.
-- * 'patchGroup' - The name of the patch group registered with the patch baseline.
-- * 'responseStatus' - The response status code.
mkRegisterPatchBaselineForPatchGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterPatchBaselineForPatchGroupResponse
mkRegisterPatchBaselineForPatchGroupResponse pResponseStatus_ =
  RegisterPatchBaselineForPatchGroupResponse'
    { baselineId =
        Lude.Nothing,
      patchGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the patch baseline the patch group was registered with.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgrsBaselineId :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse (Lude.Maybe Lude.Text)
rpbfpgrsBaselineId = Lens.lens (baselineId :: RegisterPatchBaselineForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: RegisterPatchBaselineForPatchGroupResponse)
{-# DEPRECATED rpbfpgrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group registered with the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgrsPatchGroup :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse (Lude.Maybe Lude.Text)
rpbfpgrsPatchGroup = Lens.lens (patchGroup :: RegisterPatchBaselineForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {patchGroup = a} :: RegisterPatchBaselineForPatchGroupResponse)
{-# DEPRECATED rpbfpgrsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpbfpgrsResponseStatus :: Lens.Lens' RegisterPatchBaselineForPatchGroupResponse Lude.Int
rpbfpgrsResponseStatus = Lens.lens (responseStatus :: RegisterPatchBaselineForPatchGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterPatchBaselineForPatchGroupResponse)
{-# DEPRECATED rpbfpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
