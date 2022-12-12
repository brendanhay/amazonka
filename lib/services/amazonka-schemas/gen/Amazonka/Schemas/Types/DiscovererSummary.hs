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
-- Module      : Amazonka.Schemas.Types.DiscovererSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.DiscovererSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Schemas.Types.DiscovererState

-- | /See:/ 'newDiscovererSummary' smart constructor.
data DiscovererSummary = DiscovererSummary'
  { -- | The Status if the discoverer will discover schemas from events sent from
    -- another account.
    crossAccount :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the discoverer.
    discovererArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the discoverer.
    discovererId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event bus.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the discoverer.
    state :: Prelude.Maybe DiscovererState,
    -- | Tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscovererSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossAccount', 'discovererSummary_crossAccount' - The Status if the discoverer will discover schemas from events sent from
-- another account.
--
-- 'discovererArn', 'discovererSummary_discovererArn' - The ARN of the discoverer.
--
-- 'discovererId', 'discovererSummary_discovererId' - The ID of the discoverer.
--
-- 'sourceArn', 'discovererSummary_sourceArn' - The ARN of the event bus.
--
-- 'state', 'discovererSummary_state' - The state of the discoverer.
--
-- 'tags', 'discovererSummary_tags' - Tags associated with the resource.
newDiscovererSummary ::
  DiscovererSummary
newDiscovererSummary =
  DiscovererSummary'
    { crossAccount = Prelude.Nothing,
      discovererArn = Prelude.Nothing,
      discovererId = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Status if the discoverer will discover schemas from events sent from
-- another account.
discovererSummary_crossAccount :: Lens.Lens' DiscovererSummary (Prelude.Maybe Prelude.Bool)
discovererSummary_crossAccount = Lens.lens (\DiscovererSummary' {crossAccount} -> crossAccount) (\s@DiscovererSummary' {} a -> s {crossAccount = a} :: DiscovererSummary)

-- | The ARN of the discoverer.
discovererSummary_discovererArn :: Lens.Lens' DiscovererSummary (Prelude.Maybe Prelude.Text)
discovererSummary_discovererArn = Lens.lens (\DiscovererSummary' {discovererArn} -> discovererArn) (\s@DiscovererSummary' {} a -> s {discovererArn = a} :: DiscovererSummary)

-- | The ID of the discoverer.
discovererSummary_discovererId :: Lens.Lens' DiscovererSummary (Prelude.Maybe Prelude.Text)
discovererSummary_discovererId = Lens.lens (\DiscovererSummary' {discovererId} -> discovererId) (\s@DiscovererSummary' {} a -> s {discovererId = a} :: DiscovererSummary)

-- | The ARN of the event bus.
discovererSummary_sourceArn :: Lens.Lens' DiscovererSummary (Prelude.Maybe Prelude.Text)
discovererSummary_sourceArn = Lens.lens (\DiscovererSummary' {sourceArn} -> sourceArn) (\s@DiscovererSummary' {} a -> s {sourceArn = a} :: DiscovererSummary)

-- | The state of the discoverer.
discovererSummary_state :: Lens.Lens' DiscovererSummary (Prelude.Maybe DiscovererState)
discovererSummary_state = Lens.lens (\DiscovererSummary' {state} -> state) (\s@DiscovererSummary' {} a -> s {state = a} :: DiscovererSummary)

-- | Tags associated with the resource.
discovererSummary_tags :: Lens.Lens' DiscovererSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
discovererSummary_tags = Lens.lens (\DiscovererSummary' {tags} -> tags) (\s@DiscovererSummary' {} a -> s {tags = a} :: DiscovererSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DiscovererSummary where
  parseJSON =
    Data.withObject
      "DiscovererSummary"
      ( \x ->
          DiscovererSummary'
            Prelude.<$> (x Data..:? "CrossAccount")
            Prelude.<*> (x Data..:? "DiscovererArn")
            Prelude.<*> (x Data..:? "DiscovererId")
            Prelude.<*> (x Data..:? "SourceArn")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DiscovererSummary where
  hashWithSalt _salt DiscovererSummary' {..} =
    _salt `Prelude.hashWithSalt` crossAccount
      `Prelude.hashWithSalt` discovererArn
      `Prelude.hashWithSalt` discovererId
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DiscovererSummary where
  rnf DiscovererSummary' {..} =
    Prelude.rnf crossAccount
      `Prelude.seq` Prelude.rnf discovererArn
      `Prelude.seq` Prelude.rnf discovererId
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
