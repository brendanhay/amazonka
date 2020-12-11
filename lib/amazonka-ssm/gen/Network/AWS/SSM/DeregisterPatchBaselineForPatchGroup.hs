{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a patch group from a patch baseline.
module Network.AWS.SSM.DeregisterPatchBaselineForPatchGroup
  ( -- * Creating a request
    DeregisterPatchBaselineForPatchGroup (..),
    mkDeregisterPatchBaselineForPatchGroup,

    -- ** Request lenses
    dpbfpgBaselineId,
    dpbfpgPatchGroup,

    -- * Destructuring the response
    DeregisterPatchBaselineForPatchGroupResponse (..),
    mkDeregisterPatchBaselineForPatchGroupResponse,

    -- ** Response lenses
    dpbfpgrsBaselineId,
    dpbfpgrsPatchGroup,
    dpbfpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeregisterPatchBaselineForPatchGroup' smart constructor.
data DeregisterPatchBaselineForPatchGroup = DeregisterPatchBaselineForPatchGroup'
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

-- | Creates a value of 'DeregisterPatchBaselineForPatchGroup' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline to deregister the patch group from.
-- * 'patchGroup' - The name of the patch group that should be deregistered from the patch baseline.
mkDeregisterPatchBaselineForPatchGroup ::
  -- | 'baselineId'
  Lude.Text ->
  -- | 'patchGroup'
  Lude.Text ->
  DeregisterPatchBaselineForPatchGroup
mkDeregisterPatchBaselineForPatchGroup pBaselineId_ pPatchGroup_ =
  DeregisterPatchBaselineForPatchGroup'
    { baselineId = pBaselineId_,
      patchGroup = pPatchGroup_
    }

-- | The ID of the patch baseline to deregister the patch group from.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgBaselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Lude.Text
dpbfpgBaselineId = Lens.lens (baselineId :: DeregisterPatchBaselineForPatchGroup -> Lude.Text) (\s a -> s {baselineId = a} :: DeregisterPatchBaselineForPatchGroup)
{-# DEPRECATED dpbfpgBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group that should be deregistered from the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgPatchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroup Lude.Text
dpbfpgPatchGroup = Lens.lens (patchGroup :: DeregisterPatchBaselineForPatchGroup -> Lude.Text) (\s a -> s {patchGroup = a} :: DeregisterPatchBaselineForPatchGroup)
{-# DEPRECATED dpbfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Lude.AWSRequest DeregisterPatchBaselineForPatchGroup where
  type
    Rs DeregisterPatchBaselineForPatchGroup =
      DeregisterPatchBaselineForPatchGroupResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeregisterPatchBaselineForPatchGroupResponse'
            Lude.<$> (x Lude..?> "BaselineId")
            Lude.<*> (x Lude..?> "PatchGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterPatchBaselineForPatchGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DeregisterPatchBaselineForPatchGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterPatchBaselineForPatchGroup where
  toJSON DeregisterPatchBaselineForPatchGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BaselineId" Lude..= baselineId),
            Lude.Just ("PatchGroup" Lude..= patchGroup)
          ]
      )

instance Lude.ToPath DeregisterPatchBaselineForPatchGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterPatchBaselineForPatchGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterPatchBaselineForPatchGroupResponse' smart constructor.
data DeregisterPatchBaselineForPatchGroupResponse = DeregisterPatchBaselineForPatchGroupResponse'
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

-- | Creates a value of 'DeregisterPatchBaselineForPatchGroupResponse' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the patch baseline the patch group was deregistered from.
-- * 'patchGroup' - The name of the patch group deregistered from the patch baseline.
-- * 'responseStatus' - The response status code.
mkDeregisterPatchBaselineForPatchGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterPatchBaselineForPatchGroupResponse
mkDeregisterPatchBaselineForPatchGroupResponse pResponseStatus_ =
  DeregisterPatchBaselineForPatchGroupResponse'
    { baselineId =
        Lude.Nothing,
      patchGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the patch baseline the patch group was deregistered from.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrsBaselineId :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Lude.Maybe Lude.Text)
dpbfpgrsBaselineId = Lens.lens (baselineId :: DeregisterPatchBaselineForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: DeregisterPatchBaselineForPatchGroupResponse)
{-# DEPRECATED dpbfpgrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group deregistered from the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrsPatchGroup :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse (Lude.Maybe Lude.Text)
dpbfpgrsPatchGroup = Lens.lens (patchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {patchGroup = a} :: DeregisterPatchBaselineForPatchGroupResponse)
{-# DEPRECATED dpbfpgrsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbfpgrsResponseStatus :: Lens.Lens' DeregisterPatchBaselineForPatchGroupResponse Lude.Int
dpbfpgrsResponseStatus = Lens.lens (responseStatus :: DeregisterPatchBaselineForPatchGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterPatchBaselineForPatchGroupResponse)
{-# DEPRECATED dpbfpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
