-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SampledHTTPRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SampledHTTPRequest
  ( SampledHTTPRequest (..),

    -- * Smart constructor
    mkSampledHTTPRequest,

    -- * Lenses
    shttprRuleWithinRuleGroup,
    shttprAction,
    shttprTimestamp,
    shttprRequest,
    shttprWeight,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.HTTPRequest

-- | The response from a 'GetSampledRequests' request includes a @SampledHTTPRequests@ complex type that appears as @SampledRequests@ in the response syntax. @SampledHTTPRequests@ contains one @SampledHTTPRequest@ object for each web request that is returned by @GetSampledRequests@ .
--
-- /See:/ 'mkSampledHTTPRequest' smart constructor.
data SampledHTTPRequest = SampledHTTPRequest'
  { ruleWithinRuleGroup ::
      Lude.Maybe Lude.Text,
    action :: Lude.Maybe Lude.Text,
    timestamp :: Lude.Maybe Lude.Timestamp,
    request :: HTTPRequest,
    weight :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SampledHTTPRequest' with the minimum fields required to make a request.
--
-- * 'action' - The action for the @Rule@ that the request matched: @ALLOW@ , @BLOCK@ , or @COUNT@ .
-- * 'request' - A complex type that contains detailed information about the request.
-- * 'ruleWithinRuleGroup' - This value is returned if the @GetSampledRequests@ request specifies the ID of a @RuleGroup@ rather than the ID of an individual rule. @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that matched the request listed in the response.
-- * 'timestamp' - The time at which AWS WAF received the request from your AWS resource, in Unix time format (in seconds).
-- * 'weight' - A value that indicates how one result in the response relates proportionally to other results in the response. A result that has a weight of @2@ represents roughly twice as many CloudFront web requests as a result that has a weight of @1@ .
mkSampledHTTPRequest ::
  -- | 'request'
  HTTPRequest ->
  -- | 'weight'
  Lude.Natural ->
  SampledHTTPRequest
mkSampledHTTPRequest pRequest_ pWeight_ =
  SampledHTTPRequest'
    { ruleWithinRuleGroup = Lude.Nothing,
      action = Lude.Nothing,
      timestamp = Lude.Nothing,
      request = pRequest_,
      weight = pWeight_
    }

-- | This value is returned if the @GetSampledRequests@ request specifies the ID of a @RuleGroup@ rather than the ID of an individual rule. @RuleWithinRuleGroup@ is the rule within the specified @RuleGroup@ that matched the request listed in the response.
--
-- /Note:/ Consider using 'ruleWithinRuleGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shttprRuleWithinRuleGroup :: Lens.Lens' SampledHTTPRequest (Lude.Maybe Lude.Text)
shttprRuleWithinRuleGroup = Lens.lens (ruleWithinRuleGroup :: SampledHTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {ruleWithinRuleGroup = a} :: SampledHTTPRequest)
{-# DEPRECATED shttprRuleWithinRuleGroup "Use generic-lens or generic-optics with 'ruleWithinRuleGroup' instead." #-}

-- | The action for the @Rule@ that the request matched: @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shttprAction :: Lens.Lens' SampledHTTPRequest (Lude.Maybe Lude.Text)
shttprAction = Lens.lens (action :: SampledHTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: SampledHTTPRequest)
{-# DEPRECATED shttprAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The time at which AWS WAF received the request from your AWS resource, in Unix time format (in seconds).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shttprTimestamp :: Lens.Lens' SampledHTTPRequest (Lude.Maybe Lude.Timestamp)
shttprTimestamp = Lens.lens (timestamp :: SampledHTTPRequest -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: SampledHTTPRequest)
{-# DEPRECATED shttprTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | A complex type that contains detailed information about the request.
--
-- /Note:/ Consider using 'request' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shttprRequest :: Lens.Lens' SampledHTTPRequest HTTPRequest
shttprRequest = Lens.lens (request :: SampledHTTPRequest -> HTTPRequest) (\s a -> s {request = a} :: SampledHTTPRequest)
{-# DEPRECATED shttprRequest "Use generic-lens or generic-optics with 'request' instead." #-}

-- | A value that indicates how one result in the response relates proportionally to other results in the response. A result that has a weight of @2@ represents roughly twice as many CloudFront web requests as a result that has a weight of @1@ .
--
-- /Note:/ Consider using 'weight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shttprWeight :: Lens.Lens' SampledHTTPRequest Lude.Natural
shttprWeight = Lens.lens (weight :: SampledHTTPRequest -> Lude.Natural) (\s a -> s {weight = a} :: SampledHTTPRequest)
{-# DEPRECATED shttprWeight "Use generic-lens or generic-optics with 'weight' instead." #-}

instance Lude.FromJSON SampledHTTPRequest where
  parseJSON =
    Lude.withObject
      "SampledHTTPRequest"
      ( \x ->
          SampledHTTPRequest'
            Lude.<$> (x Lude..:? "RuleWithinRuleGroup")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "Timestamp")
            Lude.<*> (x Lude..: "Request")
            Lude.<*> (x Lude..: "Weight")
      )
