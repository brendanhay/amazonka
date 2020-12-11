{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDefaultPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the default patch baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
--
-- If you do not specify an operating system value, the default patch baseline for Windows is returned.
module Network.AWS.SSM.GetDefaultPatchBaseline
  ( -- * Creating a request
    GetDefaultPatchBaseline (..),
    mkGetDefaultPatchBaseline,

    -- ** Request lenses
    gdpbOperatingSystem,

    -- * Destructuring the response
    GetDefaultPatchBaselineResponse (..),
    mkGetDefaultPatchBaselineResponse,

    -- ** Response lenses
    gdpbrsOperatingSystem,
    gdpbrsBaselineId,
    gdpbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetDefaultPatchBaseline' smart constructor.
newtype GetDefaultPatchBaseline = GetDefaultPatchBaseline'
  { operatingSystem ::
      Lude.Maybe OperatingSystem
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDefaultPatchBaseline' with the minimum fields required to make a request.
--
-- * 'operatingSystem' - Returns the default patch baseline for the specified operating system.
mkGetDefaultPatchBaseline ::
  GetDefaultPatchBaseline
mkGetDefaultPatchBaseline =
  GetDefaultPatchBaseline' {operatingSystem = Lude.Nothing}

-- | Returns the default patch baseline for the specified operating system.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbOperatingSystem :: Lens.Lens' GetDefaultPatchBaseline (Lude.Maybe OperatingSystem)
gdpbOperatingSystem = Lens.lens (operatingSystem :: GetDefaultPatchBaseline -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: GetDefaultPatchBaseline)
{-# DEPRECATED gdpbOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

instance Lude.AWSRequest GetDefaultPatchBaseline where
  type Rs GetDefaultPatchBaseline = GetDefaultPatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDefaultPatchBaselineResponse'
            Lude.<$> (x Lude..?> "OperatingSystem")
            Lude.<*> (x Lude..?> "BaselineId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDefaultPatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetDefaultPatchBaseline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDefaultPatchBaseline where
  toJSON GetDefaultPatchBaseline' {..} =
    Lude.object
      ( Lude.catMaybes
          [("OperatingSystem" Lude..=) Lude.<$> operatingSystem]
      )

instance Lude.ToPath GetDefaultPatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDefaultPatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDefaultPatchBaselineResponse' smart constructor.
data GetDefaultPatchBaselineResponse = GetDefaultPatchBaselineResponse'
  { operatingSystem ::
      Lude.Maybe OperatingSystem,
    baselineId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDefaultPatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the default patch baseline.
-- * 'operatingSystem' - The operating system for the returned patch baseline.
-- * 'responseStatus' - The response status code.
mkGetDefaultPatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDefaultPatchBaselineResponse
mkGetDefaultPatchBaselineResponse pResponseStatus_ =
  GetDefaultPatchBaselineResponse'
    { operatingSystem = Lude.Nothing,
      baselineId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The operating system for the returned patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbrsOperatingSystem :: Lens.Lens' GetDefaultPatchBaselineResponse (Lude.Maybe OperatingSystem)
gdpbrsOperatingSystem = Lens.lens (operatingSystem :: GetDefaultPatchBaselineResponse -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: GetDefaultPatchBaselineResponse)
{-# DEPRECATED gdpbrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The ID of the default patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbrsBaselineId :: Lens.Lens' GetDefaultPatchBaselineResponse (Lude.Maybe Lude.Text)
gdpbrsBaselineId = Lens.lens (baselineId :: GetDefaultPatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: GetDefaultPatchBaselineResponse)
{-# DEPRECATED gdpbrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpbrsResponseStatus :: Lens.Lens' GetDefaultPatchBaselineResponse Lude.Int
gdpbrsResponseStatus = Lens.lens (responseStatus :: GetDefaultPatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDefaultPatchBaselineResponse)
{-# DEPRECATED gdpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
