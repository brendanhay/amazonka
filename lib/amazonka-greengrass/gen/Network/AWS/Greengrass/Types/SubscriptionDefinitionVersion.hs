{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
  ( SubscriptionDefinitionVersion (..),

    -- * Smart constructor
    mkSubscriptionDefinitionVersion,

    -- * Lenses
    sdvSubscriptions,
  )
where

import Network.AWS.Greengrass.Types.Subscription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a subscription definition version.
--
-- /See:/ 'mkSubscriptionDefinitionVersion' smart constructor.
newtype SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { subscriptions ::
      Lude.Maybe [Subscription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscriptionDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'subscriptions' - A list of subscriptions.
mkSubscriptionDefinitionVersion ::
  SubscriptionDefinitionVersion
mkSubscriptionDefinitionVersion =
  SubscriptionDefinitionVersion' {subscriptions = Lude.Nothing}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdvSubscriptions :: Lens.Lens' SubscriptionDefinitionVersion (Lude.Maybe [Subscription])
sdvSubscriptions = Lens.lens (subscriptions :: SubscriptionDefinitionVersion -> Lude.Maybe [Subscription]) (\s a -> s {subscriptions = a} :: SubscriptionDefinitionVersion)
{-# DEPRECATED sdvSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

instance Lude.FromJSON SubscriptionDefinitionVersion where
  parseJSON =
    Lude.withObject
      "SubscriptionDefinitionVersion"
      ( \x ->
          SubscriptionDefinitionVersion'
            Lude.<$> (x Lude..:? "Subscriptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SubscriptionDefinitionVersion where
  toJSON SubscriptionDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Subscriptions" Lude..=) Lude.<$> subscriptions])
