{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.Subscription
import qualified Network.AWS.Lens as Lens

-- | Information about a subscription definition version.
--
-- /See:/ 'newSubscriptionDefinitionVersion' smart constructor.
data SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Subscription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubscriptionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptions', 'subscriptionDefinitionVersion_subscriptions' - A list of subscriptions.
newSubscriptionDefinitionVersion ::
  SubscriptionDefinitionVersion
newSubscriptionDefinitionVersion =
  SubscriptionDefinitionVersion'
    { subscriptions =
        Core.Nothing
    }

-- | A list of subscriptions.
subscriptionDefinitionVersion_subscriptions :: Lens.Lens' SubscriptionDefinitionVersion (Core.Maybe [Subscription])
subscriptionDefinitionVersion_subscriptions = Lens.lens (\SubscriptionDefinitionVersion' {subscriptions} -> subscriptions) (\s@SubscriptionDefinitionVersion' {} a -> s {subscriptions = a} :: SubscriptionDefinitionVersion) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SubscriptionDefinitionVersion where
  parseJSON =
    Core.withObject
      "SubscriptionDefinitionVersion"
      ( \x ->
          SubscriptionDefinitionVersion'
            Core.<$> (x Core..:? "Subscriptions" Core..!= Core.mempty)
      )

instance Core.Hashable SubscriptionDefinitionVersion

instance Core.NFData SubscriptionDefinitionVersion

instance Core.ToJSON SubscriptionDefinitionVersion where
  toJSON SubscriptionDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Subscriptions" Core..=) Core.<$> subscriptions]
      )
