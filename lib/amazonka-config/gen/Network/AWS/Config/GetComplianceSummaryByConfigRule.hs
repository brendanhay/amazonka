{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceSummaryByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of AWS Config rules that are compliant and noncompliant, up to a maximum of 25 for each.
module Network.AWS.Config.GetComplianceSummaryByConfigRule
  ( -- * Creating a request
    GetComplianceSummaryByConfigRule (..),
    mkGetComplianceSummaryByConfigRule,

    -- * Destructuring the response
    GetComplianceSummaryByConfigRuleResponse (..),
    mkGetComplianceSummaryByConfigRuleResponse,

    -- ** Response lenses
    gcsbcrrsComplianceSummary,
    gcsbcrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetComplianceSummaryByConfigRule' smart constructor.
data GetComplianceSummaryByConfigRule = GetComplianceSummaryByConfigRule'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceSummaryByConfigRule' with the minimum fields required to make a request.
mkGetComplianceSummaryByConfigRule ::
  GetComplianceSummaryByConfigRule
mkGetComplianceSummaryByConfigRule =
  GetComplianceSummaryByConfigRule'

instance Lude.AWSRequest GetComplianceSummaryByConfigRule where
  type
    Rs GetComplianceSummaryByConfigRule =
      GetComplianceSummaryByConfigRuleResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetComplianceSummaryByConfigRuleResponse'
            Lude.<$> (x Lude..?> "ComplianceSummary")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComplianceSummaryByConfigRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetComplianceSummaryByConfigRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComplianceSummaryByConfigRule where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetComplianceSummaryByConfigRule where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComplianceSummaryByConfigRule where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkGetComplianceSummaryByConfigRuleResponse' smart constructor.
data GetComplianceSummaryByConfigRuleResponse = GetComplianceSummaryByConfigRuleResponse'
  { complianceSummary ::
      Lude.Maybe
        ComplianceSummary,
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

-- | Creates a value of 'GetComplianceSummaryByConfigRuleResponse' with the minimum fields required to make a request.
--
-- * 'complianceSummary' - The number of AWS Config rules that are compliant and the number that are noncompliant, up to a maximum of 25 for each.
-- * 'responseStatus' - The response status code.
mkGetComplianceSummaryByConfigRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetComplianceSummaryByConfigRuleResponse
mkGetComplianceSummaryByConfigRuleResponse pResponseStatus_ =
  GetComplianceSummaryByConfigRuleResponse'
    { complianceSummary =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of AWS Config rules that are compliant and the number that are noncompliant, up to a maximum of 25 for each.
--
-- /Note:/ Consider using 'complianceSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbcrrsComplianceSummary :: Lens.Lens' GetComplianceSummaryByConfigRuleResponse (Lude.Maybe ComplianceSummary)
gcsbcrrsComplianceSummary = Lens.lens (complianceSummary :: GetComplianceSummaryByConfigRuleResponse -> Lude.Maybe ComplianceSummary) (\s a -> s {complianceSummary = a} :: GetComplianceSummaryByConfigRuleResponse)
{-# DEPRECATED gcsbcrrsComplianceSummary "Use generic-lens or generic-optics with 'complianceSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbcrrsResponseStatus :: Lens.Lens' GetComplianceSummaryByConfigRuleResponse Lude.Int
gcsbcrrsResponseStatus = Lens.lens (responseStatus :: GetComplianceSummaryByConfigRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetComplianceSummaryByConfigRuleResponse)
{-# DEPRECATED gcsbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
