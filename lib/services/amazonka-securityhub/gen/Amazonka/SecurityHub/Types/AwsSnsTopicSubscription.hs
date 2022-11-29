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
-- Module      : Amazonka.SecurityHub.Types.AwsSnsTopicSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSnsTopicSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
-- /See:/ 'newAwsSnsTopicSubscription' smart constructor.
data AwsSnsTopicSubscription = AwsSnsTopicSubscription'
  { -- | The subscription\'s protocol.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The subscription\'s endpoint (format depends on the protocol).
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSnsTopicSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'awsSnsTopicSubscription_protocol' - The subscription\'s protocol.
--
-- 'endpoint', 'awsSnsTopicSubscription_endpoint' - The subscription\'s endpoint (format depends on the protocol).
newAwsSnsTopicSubscription ::
  AwsSnsTopicSubscription
newAwsSnsTopicSubscription =
  AwsSnsTopicSubscription'
    { protocol =
        Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The subscription\'s protocol.
awsSnsTopicSubscription_protocol :: Lens.Lens' AwsSnsTopicSubscription (Prelude.Maybe Prelude.Text)
awsSnsTopicSubscription_protocol = Lens.lens (\AwsSnsTopicSubscription' {protocol} -> protocol) (\s@AwsSnsTopicSubscription' {} a -> s {protocol = a} :: AwsSnsTopicSubscription)

-- | The subscription\'s endpoint (format depends on the protocol).
awsSnsTopicSubscription_endpoint :: Lens.Lens' AwsSnsTopicSubscription (Prelude.Maybe Prelude.Text)
awsSnsTopicSubscription_endpoint = Lens.lens (\AwsSnsTopicSubscription' {endpoint} -> endpoint) (\s@AwsSnsTopicSubscription' {} a -> s {endpoint = a} :: AwsSnsTopicSubscription)

instance Core.FromJSON AwsSnsTopicSubscription where
  parseJSON =
    Core.withObject
      "AwsSnsTopicSubscription"
      ( \x ->
          AwsSnsTopicSubscription'
            Prelude.<$> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "Endpoint")
      )

instance Prelude.Hashable AwsSnsTopicSubscription where
  hashWithSalt _salt AwsSnsTopicSubscription' {..} =
    _salt `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData AwsSnsTopicSubscription where
  rnf AwsSnsTopicSubscription' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf endpoint

instance Core.ToJSON AwsSnsTopicSubscription where
  toJSON AwsSnsTopicSubscription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Protocol" Core..=) Prelude.<$> protocol,
            ("Endpoint" Core..=) Prelude.<$> endpoint
          ]
      )
