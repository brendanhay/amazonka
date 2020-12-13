{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of IP addresses currently being blocked by the 'RateBasedRule' that is specified by the @RuleId@ . The maximum number of managed keys that will be blocked is 10,000. If more than 10,000 addresses exceed the rate limit, the 10,000 addresses with the highest rates will be blocked.
module Network.AWS.WAFRegional.GetRateBasedRuleManagedKeys
  ( -- * Creating a request
    GetRateBasedRuleManagedKeys (..),
    mkGetRateBasedRuleManagedKeys,

    -- ** Request lenses
    grbrmkRuleId,
    grbrmkNextMarker,

    -- * Destructuring the response
    GetRateBasedRuleManagedKeysResponse (..),
    mkGetRateBasedRuleManagedKeysResponse,

    -- ** Response lenses
    grbrmkrsNextMarker,
    grbrmkrsManagedKeys,
    grbrmkrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetRateBasedRuleManagedKeys' smart constructor.
data GetRateBasedRuleManagedKeys = GetRateBasedRuleManagedKeys'
  { -- | The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
    ruleId :: Lude.Text,
    -- | A null value and not currently used. Do not include this in your request.
    nextMarker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRateBasedRuleManagedKeys' with the minimum fields required to make a request.
--
-- * 'ruleId' - The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
-- * 'nextMarker' - A null value and not currently used. Do not include this in your request.
mkGetRateBasedRuleManagedKeys ::
  -- | 'ruleId'
  Lude.Text ->
  GetRateBasedRuleManagedKeys
mkGetRateBasedRuleManagedKeys pRuleId_ =
  GetRateBasedRuleManagedKeys'
    { ruleId = pRuleId_,
      nextMarker = Lude.Nothing
    }

-- | The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkRuleId :: Lens.Lens' GetRateBasedRuleManagedKeys Lude.Text
grbrmkRuleId = Lens.lens (ruleId :: GetRateBasedRuleManagedKeys -> Lude.Text) (\s a -> s {ruleId = a} :: GetRateBasedRuleManagedKeys)
{-# DEPRECATED grbrmkRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | A null value and not currently used. Do not include this in your request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkNextMarker :: Lens.Lens' GetRateBasedRuleManagedKeys (Lude.Maybe Lude.Text)
grbrmkNextMarker = Lens.lens (nextMarker :: GetRateBasedRuleManagedKeys -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: GetRateBasedRuleManagedKeys)
{-# DEPRECATED grbrmkNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Lude.AWSRequest GetRateBasedRuleManagedKeys where
  type
    Rs GetRateBasedRuleManagedKeys =
      GetRateBasedRuleManagedKeysResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRateBasedRuleManagedKeysResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "ManagedKeys" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRateBasedRuleManagedKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.GetRateBasedRuleManagedKeys" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRateBasedRuleManagedKeys where
  toJSON GetRateBasedRuleManagedKeys' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuleId" Lude..= ruleId),
            ("NextMarker" Lude..=) Lude.<$> nextMarker
          ]
      )

instance Lude.ToPath GetRateBasedRuleManagedKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRateBasedRuleManagedKeys where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRateBasedRuleManagedKeysResponse' smart constructor.
data GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse'
  { -- | A null value and not currently used.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | An array of IP addresses that currently are blocked by the specified 'RateBasedRule' .
    managedKeys :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRateBasedRuleManagedKeysResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - A null value and not currently used.
-- * 'managedKeys' - An array of IP addresses that currently are blocked by the specified 'RateBasedRule' .
-- * 'responseStatus' - The response status code.
mkGetRateBasedRuleManagedKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRateBasedRuleManagedKeysResponse
mkGetRateBasedRuleManagedKeysResponse pResponseStatus_ =
  GetRateBasedRuleManagedKeysResponse'
    { nextMarker = Lude.Nothing,
      managedKeys = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A null value and not currently used.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkrsNextMarker :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Lude.Maybe Lude.Text)
grbrmkrsNextMarker = Lens.lens (nextMarker :: GetRateBasedRuleManagedKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: GetRateBasedRuleManagedKeysResponse)
{-# DEPRECATED grbrmkrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of IP addresses that currently are blocked by the specified 'RateBasedRule' .
--
-- /Note:/ Consider using 'managedKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkrsManagedKeys :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Lude.Maybe [Lude.Text])
grbrmkrsManagedKeys = Lens.lens (managedKeys :: GetRateBasedRuleManagedKeysResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {managedKeys = a} :: GetRateBasedRuleManagedKeysResponse)
{-# DEPRECATED grbrmkrsManagedKeys "Use generic-lens or generic-optics with 'managedKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkrsResponseStatus :: Lens.Lens' GetRateBasedRuleManagedKeysResponse Lude.Int
grbrmkrsResponseStatus = Lens.lens (responseStatus :: GetRateBasedRuleManagedKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRateBasedRuleManagedKeysResponse)
{-# DEPRECATED grbrmkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
