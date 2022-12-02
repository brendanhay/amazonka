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
-- Module      : Amazonka.Greengrass.Types.SubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.SubscriptionDefinitionVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.Subscription
import qualified Amazonka.Prelude as Prelude

-- | Information about a subscription definition version.
--
-- /See:/ 'newSubscriptionDefinitionVersion' smart constructor.
data SubscriptionDefinitionVersion = SubscriptionDefinitionVersion'
  { -- | A list of subscriptions.
    subscriptions :: Prelude.Maybe [Subscription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | A list of subscriptions.
subscriptionDefinitionVersion_subscriptions :: Lens.Lens' SubscriptionDefinitionVersion (Prelude.Maybe [Subscription])
subscriptionDefinitionVersion_subscriptions = Lens.lens (\SubscriptionDefinitionVersion' {subscriptions} -> subscriptions) (\s@SubscriptionDefinitionVersion' {} a -> s {subscriptions = a} :: SubscriptionDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SubscriptionDefinitionVersion where
  parseJSON =
    Data.withObject
      "SubscriptionDefinitionVersion"
      ( \x ->
          SubscriptionDefinitionVersion'
            Prelude.<$> (x Data..:? "Subscriptions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    SubscriptionDefinitionVersion
  where
  hashWithSalt _salt SubscriptionDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` subscriptions

instance Prelude.NFData SubscriptionDefinitionVersion where
  rnf SubscriptionDefinitionVersion' {..} =
    Prelude.rnf subscriptions

instance Data.ToJSON SubscriptionDefinitionVersion where
  toJSON SubscriptionDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Subscriptions" Data..=)
              Prelude.<$> subscriptions
          ]
      )
