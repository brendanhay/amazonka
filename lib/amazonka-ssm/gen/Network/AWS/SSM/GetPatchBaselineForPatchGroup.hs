{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the patch baseline that should be used for the specified patch group.
module Network.AWS.SSM.GetPatchBaselineForPatchGroup
  ( -- * Creating a request
    GetPatchBaselineForPatchGroup (..),
    mkGetPatchBaselineForPatchGroup,

    -- ** Request lenses
    gpbfpgOperatingSystem,
    gpbfpgPatchGroup,

    -- * Destructuring the response
    GetPatchBaselineForPatchGroupResponse (..),
    mkGetPatchBaselineForPatchGroupResponse,

    -- ** Response lenses
    gpbfpgrsOperatingSystem,
    gpbfpgrsBaselineId,
    gpbfpgrsPatchGroup,
    gpbfpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetPatchBaselineForPatchGroup' smart constructor.
data GetPatchBaselineForPatchGroup = GetPatchBaselineForPatchGroup'
  { -- | Returns he operating system rule specified for patch groups using the patch baseline.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | The name of the patch group whose patch baseline should be retrieved.
    patchGroup :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPatchBaselineForPatchGroup' with the minimum fields required to make a request.
--
-- * 'operatingSystem' - Returns he operating system rule specified for patch groups using the patch baseline.
-- * 'patchGroup' - The name of the patch group whose patch baseline should be retrieved.
mkGetPatchBaselineForPatchGroup ::
  -- | 'patchGroup'
  Lude.Text ->
  GetPatchBaselineForPatchGroup
mkGetPatchBaselineForPatchGroup pPatchGroup_ =
  GetPatchBaselineForPatchGroup'
    { operatingSystem = Lude.Nothing,
      patchGroup = pPatchGroup_
    }

-- | Returns he operating system rule specified for patch groups using the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgOperatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroup (Lude.Maybe OperatingSystem)
gpbfpgOperatingSystem = Lens.lens (operatingSystem :: GetPatchBaselineForPatchGroup -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: GetPatchBaselineForPatchGroup)
{-# DEPRECATED gpbfpgOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The name of the patch group whose patch baseline should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgPatchGroup :: Lens.Lens' GetPatchBaselineForPatchGroup Lude.Text
gpbfpgPatchGroup = Lens.lens (patchGroup :: GetPatchBaselineForPatchGroup -> Lude.Text) (\s a -> s {patchGroup = a} :: GetPatchBaselineForPatchGroup)
{-# DEPRECATED gpbfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Lude.AWSRequest GetPatchBaselineForPatchGroup where
  type
    Rs GetPatchBaselineForPatchGroup =
      GetPatchBaselineForPatchGroupResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPatchBaselineForPatchGroupResponse'
            Lude.<$> (x Lude..?> "OperatingSystem")
            Lude.<*> (x Lude..?> "BaselineId")
            Lude.<*> (x Lude..?> "PatchGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPatchBaselineForPatchGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetPatchBaselineForPatchGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPatchBaselineForPatchGroup where
  toJSON GetPatchBaselineForPatchGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OperatingSystem" Lude..=) Lude.<$> operatingSystem,
            Lude.Just ("PatchGroup" Lude..= patchGroup)
          ]
      )

instance Lude.ToPath GetPatchBaselineForPatchGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPatchBaselineForPatchGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPatchBaselineForPatchGroupResponse' smart constructor.
data GetPatchBaselineForPatchGroupResponse = GetPatchBaselineForPatchGroupResponse'
  { -- | The operating system rule specified for patch groups using the patch baseline.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | The ID of the patch baseline that should be used for the patch group.
    baselineId :: Lude.Maybe Lude.Text,
    -- | The name of the patch group.
    patchGroup :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPatchBaselineForPatchGroupResponse' with the minimum fields required to make a request.
--
-- * 'operatingSystem' - The operating system rule specified for patch groups using the patch baseline.
-- * 'baselineId' - The ID of the patch baseline that should be used for the patch group.
-- * 'patchGroup' - The name of the patch group.
-- * 'responseStatus' - The response status code.
mkGetPatchBaselineForPatchGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPatchBaselineForPatchGroupResponse
mkGetPatchBaselineForPatchGroupResponse pResponseStatus_ =
  GetPatchBaselineForPatchGroupResponse'
    { operatingSystem =
        Lude.Nothing,
      baselineId = Lude.Nothing,
      patchGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The operating system rule specified for patch groups using the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrsOperatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Lude.Maybe OperatingSystem)
gpbfpgrsOperatingSystem = Lens.lens (operatingSystem :: GetPatchBaselineForPatchGroupResponse -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: GetPatchBaselineForPatchGroupResponse)
{-# DEPRECATED gpbfpgrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The ID of the patch baseline that should be used for the patch group.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrsBaselineId :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Lude.Maybe Lude.Text)
gpbfpgrsBaselineId = Lens.lens (baselineId :: GetPatchBaselineForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: GetPatchBaselineForPatchGroupResponse)
{-# DEPRECATED gpbfpgrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch group.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrsPatchGroup :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Lude.Maybe Lude.Text)
gpbfpgrsPatchGroup = Lens.lens (patchGroup :: GetPatchBaselineForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {patchGroup = a} :: GetPatchBaselineForPatchGroupResponse)
{-# DEPRECATED gpbfpgrsPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrsResponseStatus :: Lens.Lens' GetPatchBaselineForPatchGroupResponse Lude.Int
gpbfpgrsResponseStatus = Lens.lens (responseStatus :: GetPatchBaselineForPatchGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPatchBaselineForPatchGroupResponse)
{-# DEPRECATED gpbfpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
