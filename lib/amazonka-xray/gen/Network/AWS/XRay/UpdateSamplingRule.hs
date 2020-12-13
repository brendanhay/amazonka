{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.UpdateSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a sampling rule's configuration.
module Network.AWS.XRay.UpdateSamplingRule
  ( -- * Creating a request
    UpdateSamplingRule (..),
    mkUpdateSamplingRule,

    -- ** Request lenses
    usrSamplingRuleUpdate,

    -- * Destructuring the response
    UpdateSamplingRuleResponse (..),
    mkUpdateSamplingRuleResponse,

    -- ** Response lenses
    usrrsSamplingRuleRecord,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkUpdateSamplingRule' smart constructor.
newtype UpdateSamplingRule = UpdateSamplingRule'
  { -- | The rule and fields to change.
    samplingRuleUpdate :: SamplingRuleUpdate
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSamplingRule' with the minimum fields required to make a request.
--
-- * 'samplingRuleUpdate' - The rule and fields to change.
mkUpdateSamplingRule ::
  -- | 'samplingRuleUpdate'
  SamplingRuleUpdate ->
  UpdateSamplingRule
mkUpdateSamplingRule pSamplingRuleUpdate_ =
  UpdateSamplingRule' {samplingRuleUpdate = pSamplingRuleUpdate_}

-- | The rule and fields to change.
--
-- /Note:/ Consider using 'samplingRuleUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrSamplingRuleUpdate :: Lens.Lens' UpdateSamplingRule SamplingRuleUpdate
usrSamplingRuleUpdate = Lens.lens (samplingRuleUpdate :: UpdateSamplingRule -> SamplingRuleUpdate) (\s a -> s {samplingRuleUpdate = a} :: UpdateSamplingRule)
{-# DEPRECATED usrSamplingRuleUpdate "Use generic-lens or generic-optics with 'samplingRuleUpdate' instead." #-}

instance Lude.AWSRequest UpdateSamplingRule where
  type Rs UpdateSamplingRule = UpdateSamplingRuleResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSamplingRuleResponse'
            Lude.<$> (x Lude..?> "SamplingRuleRecord")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSamplingRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateSamplingRule where
  toJSON UpdateSamplingRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SamplingRuleUpdate" Lude..= samplingRuleUpdate)]
      )

instance Lude.ToPath UpdateSamplingRule where
  toPath = Lude.const "/UpdateSamplingRule"

instance Lude.ToQuery UpdateSamplingRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSamplingRuleResponse' smart constructor.
data UpdateSamplingRuleResponse = UpdateSamplingRuleResponse'
  { -- | The updated rule definition and metadata.
    samplingRuleRecord :: Lude.Maybe SamplingRuleRecord,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSamplingRuleResponse' with the minimum fields required to make a request.
--
-- * 'samplingRuleRecord' - The updated rule definition and metadata.
-- * 'responseStatus' - The response status code.
mkUpdateSamplingRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSamplingRuleResponse
mkUpdateSamplingRuleResponse pResponseStatus_ =
  UpdateSamplingRuleResponse'
    { samplingRuleRecord = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSamplingRuleRecord :: Lens.Lens' UpdateSamplingRuleResponse (Lude.Maybe SamplingRuleRecord)
usrrsSamplingRuleRecord = Lens.lens (samplingRuleRecord :: UpdateSamplingRuleResponse -> Lude.Maybe SamplingRuleRecord) (\s a -> s {samplingRuleRecord = a} :: UpdateSamplingRuleResponse)
{-# DEPRECATED usrrsSamplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSamplingRuleResponse Lude.Int
usrrsResponseStatus = Lens.lens (responseStatus :: UpdateSamplingRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSamplingRuleResponse)
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
