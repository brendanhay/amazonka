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
-- Module      : Amazonka.PinpointEmail.Types.InboxPlacementTrackingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.InboxPlacementTrackingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the inbox placement data
-- settings for a verified domain that’s associated with your AWS account.
-- This data is available only if you enabled the Deliverability dashboard
-- for the domain (@PutDeliverabilityDashboardOption@ operation).
--
-- /See:/ 'newInboxPlacementTrackingOption' smart constructor.
data InboxPlacementTrackingOption = InboxPlacementTrackingOption'
  { -- | Specifies whether inbox placement data is being tracked for the domain.
    global :: Prelude.Maybe Prelude.Bool,
    -- | An array of strings, one for each major email provider that the inbox
    -- placement data applies to.
    trackedIsps :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InboxPlacementTrackingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'global', 'inboxPlacementTrackingOption_global' - Specifies whether inbox placement data is being tracked for the domain.
--
-- 'trackedIsps', 'inboxPlacementTrackingOption_trackedIsps' - An array of strings, one for each major email provider that the inbox
-- placement data applies to.
newInboxPlacementTrackingOption ::
  InboxPlacementTrackingOption
newInboxPlacementTrackingOption =
  InboxPlacementTrackingOption'
    { global =
        Prelude.Nothing,
      trackedIsps = Prelude.Nothing
    }

-- | Specifies whether inbox placement data is being tracked for the domain.
inboxPlacementTrackingOption_global :: Lens.Lens' InboxPlacementTrackingOption (Prelude.Maybe Prelude.Bool)
inboxPlacementTrackingOption_global = Lens.lens (\InboxPlacementTrackingOption' {global} -> global) (\s@InboxPlacementTrackingOption' {} a -> s {global = a} :: InboxPlacementTrackingOption)

-- | An array of strings, one for each major email provider that the inbox
-- placement data applies to.
inboxPlacementTrackingOption_trackedIsps :: Lens.Lens' InboxPlacementTrackingOption (Prelude.Maybe [Prelude.Text])
inboxPlacementTrackingOption_trackedIsps = Lens.lens (\InboxPlacementTrackingOption' {trackedIsps} -> trackedIsps) (\s@InboxPlacementTrackingOption' {} a -> s {trackedIsps = a} :: InboxPlacementTrackingOption) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InboxPlacementTrackingOption where
  parseJSON =
    Data.withObject
      "InboxPlacementTrackingOption"
      ( \x ->
          InboxPlacementTrackingOption'
            Prelude.<$> (x Data..:? "Global")
            Prelude.<*> (x Data..:? "TrackedIsps" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    InboxPlacementTrackingOption
  where
  hashWithSalt _salt InboxPlacementTrackingOption' {..} =
    _salt
      `Prelude.hashWithSalt` global
      `Prelude.hashWithSalt` trackedIsps

instance Prelude.NFData InboxPlacementTrackingOption where
  rnf InboxPlacementTrackingOption' {..} =
    Prelude.rnf global
      `Prelude.seq` Prelude.rnf trackedIsps

instance Data.ToJSON InboxPlacementTrackingOption where
  toJSON InboxPlacementTrackingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Global" Data..=) Prelude.<$> global,
            ("TrackedIsps" Data..=) Prelude.<$> trackedIsps
          ]
      )
