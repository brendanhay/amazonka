{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.DeleteSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a sampling rule.
module Network.AWS.XRay.DeleteSamplingRule
  ( -- * Creating a request
    DeleteSamplingRule (..),
    mkDeleteSamplingRule,

    -- ** Request lenses
    dsrRuleName,
    dsrRuleARN,

    -- * Destructuring the response
    DeleteSamplingRuleResponse (..),
    mkDeleteSamplingRuleResponse,

    -- ** Response lenses
    dsrrsSamplingRuleRecord,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkDeleteSamplingRule' smart constructor.
data DeleteSamplingRule = DeleteSamplingRule'
  { ruleName ::
      Lude.Maybe Lude.Text,
    ruleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSamplingRule' with the minimum fields required to make a request.
--
-- * 'ruleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
-- * 'ruleName' - The name of the sampling rule. Specify a rule by either name or ARN, but not both.
mkDeleteSamplingRule ::
  DeleteSamplingRule
mkDeleteSamplingRule =
  DeleteSamplingRule'
    { ruleName = Lude.Nothing,
      ruleARN = Lude.Nothing
    }

-- | The name of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRuleName :: Lens.Lens' DeleteSamplingRule (Lude.Maybe Lude.Text)
dsrRuleName = Lens.lens (ruleName :: DeleteSamplingRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: DeleteSamplingRule)
{-# DEPRECATED dsrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but not both.
--
-- /Note:/ Consider using 'ruleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRuleARN :: Lens.Lens' DeleteSamplingRule (Lude.Maybe Lude.Text)
dsrRuleARN = Lens.lens (ruleARN :: DeleteSamplingRule -> Lude.Maybe Lude.Text) (\s a -> s {ruleARN = a} :: DeleteSamplingRule)
{-# DEPRECATED dsrRuleARN "Use generic-lens or generic-optics with 'ruleARN' instead." #-}

instance Lude.AWSRequest DeleteSamplingRule where
  type Rs DeleteSamplingRule = DeleteSamplingRuleResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSamplingRuleResponse'
            Lude.<$> (x Lude..?> "SamplingRuleRecord")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSamplingRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeleteSamplingRule where
  toJSON DeleteSamplingRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RuleName" Lude..=) Lude.<$> ruleName,
            ("RuleARN" Lude..=) Lude.<$> ruleARN
          ]
      )

instance Lude.ToPath DeleteSamplingRule where
  toPath = Lude.const "/DeleteSamplingRule"

instance Lude.ToQuery DeleteSamplingRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSamplingRuleResponse' smart constructor.
data DeleteSamplingRuleResponse = DeleteSamplingRuleResponse'
  { samplingRuleRecord ::
      Lude.Maybe SamplingRuleRecord,
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

-- | Creates a value of 'DeleteSamplingRuleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'samplingRuleRecord' - The deleted rule definition and metadata.
mkDeleteSamplingRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSamplingRuleResponse
mkDeleteSamplingRuleResponse pResponseStatus_ =
  DeleteSamplingRuleResponse'
    { samplingRuleRecord = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The deleted rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSamplingRuleRecord :: Lens.Lens' DeleteSamplingRuleResponse (Lude.Maybe SamplingRuleRecord)
dsrrsSamplingRuleRecord = Lens.lens (samplingRuleRecord :: DeleteSamplingRuleResponse -> Lude.Maybe SamplingRuleRecord) (\s a -> s {samplingRuleRecord = a} :: DeleteSamplingRuleResponse)
{-# DEPRECATED dsrrsSamplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSamplingRuleResponse Lude.Int
dsrrsResponseStatus = Lens.lens (responseStatus :: DeleteSamplingRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSamplingRuleResponse)
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
