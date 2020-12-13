{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a usage limit in a cluster. You can't modify the feature type or period of a usage limit.
module Network.AWS.Redshift.ModifyUsageLimit
  ( -- * Creating a request
    ModifyUsageLimit (..),
    mkModifyUsageLimit,

    -- ** Request lenses
    mulAmount,
    mulUsageLimitId,
    mulBreachAction,

    -- * Destructuring the response
    UsageLimit (..),
    mkUsageLimit,

    -- ** Response lenses
    ulAmount,
    ulLimitType,
    ulUsageLimitId,
    ulPeriod,
    ulClusterIdentifier,
    ulBreachAction,
    ulFeatureType,
    ulTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyUsageLimit' smart constructor.
data ModifyUsageLimit = ModifyUsageLimit'
  { -- | The new limit amount. For more information about this parameter, see 'UsageLimit' .
    amount :: Lude.Maybe Lude.Integer,
    -- | The identifier of the usage limit to modify.
    usageLimitId :: Lude.Text,
    -- | The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' .
    breachAction :: Lude.Maybe UsageLimitBreachAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyUsageLimit' with the minimum fields required to make a request.
--
-- * 'amount' - The new limit amount. For more information about this parameter, see 'UsageLimit' .
-- * 'usageLimitId' - The identifier of the usage limit to modify.
-- * 'breachAction' - The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' .
mkModifyUsageLimit ::
  -- | 'usageLimitId'
  Lude.Text ->
  ModifyUsageLimit
mkModifyUsageLimit pUsageLimitId_ =
  ModifyUsageLimit'
    { amount = Lude.Nothing,
      usageLimitId = pUsageLimitId_,
      breachAction = Lude.Nothing
    }

-- | The new limit amount. For more information about this parameter, see 'UsageLimit' .
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mulAmount :: Lens.Lens' ModifyUsageLimit (Lude.Maybe Lude.Integer)
mulAmount = Lens.lens (amount :: ModifyUsageLimit -> Lude.Maybe Lude.Integer) (\s a -> s {amount = a} :: ModifyUsageLimit)
{-# DEPRECATED mulAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The identifier of the usage limit to modify.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mulUsageLimitId :: Lens.Lens' ModifyUsageLimit Lude.Text
mulUsageLimitId = Lens.lens (usageLimitId :: ModifyUsageLimit -> Lude.Text) (\s a -> s {usageLimitId = a} :: ModifyUsageLimit)
{-# DEPRECATED mulUsageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead." #-}

-- | The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' .
--
-- /Note:/ Consider using 'breachAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mulBreachAction :: Lens.Lens' ModifyUsageLimit (Lude.Maybe UsageLimitBreachAction)
mulBreachAction = Lens.lens (breachAction :: ModifyUsageLimit -> Lude.Maybe UsageLimitBreachAction) (\s a -> s {breachAction = a} :: ModifyUsageLimit)
{-# DEPRECATED mulBreachAction "Use generic-lens or generic-optics with 'breachAction' instead." #-}

instance Lude.AWSRequest ModifyUsageLimit where
  type Rs ModifyUsageLimit = UsageLimit
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyUsageLimitResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyUsageLimit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyUsageLimit where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyUsageLimit where
  toQuery ModifyUsageLimit' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyUsageLimit" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Amount" Lude.=: amount,
        "UsageLimitId" Lude.=: usageLimitId,
        "BreachAction" Lude.=: breachAction
      ]
