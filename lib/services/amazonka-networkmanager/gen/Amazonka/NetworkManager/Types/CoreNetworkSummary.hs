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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.CoreNetworkState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Returns summary information about a core network.
--
-- /See:/ 'newCoreNetworkSummary' smart constructor.
data CoreNetworkSummary = CoreNetworkSummary'
  { -- | a core network ARN.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The description of a core network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The global network ID.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The state of a core network.
    state :: Prelude.Maybe CoreNetworkState,
    -- | The key-value tags associated with a core network summary.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkArn', 'coreNetworkSummary_coreNetworkArn' - a core network ARN.
--
-- 'coreNetworkId', 'coreNetworkSummary_coreNetworkId' - The ID of a core network.
--
-- 'description', 'coreNetworkSummary_description' - The description of a core network.
--
-- 'globalNetworkId', 'coreNetworkSummary_globalNetworkId' - The global network ID.
--
-- 'ownerAccountId', 'coreNetworkSummary_ownerAccountId' - The ID of the account owner.
--
-- 'state', 'coreNetworkSummary_state' - The state of a core network.
--
-- 'tags', 'coreNetworkSummary_tags' - The key-value tags associated with a core network summary.
newCoreNetworkSummary ::
  CoreNetworkSummary
newCoreNetworkSummary =
  CoreNetworkSummary'
    { coreNetworkArn =
        Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      description = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | a core network ARN.
coreNetworkSummary_coreNetworkArn :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_coreNetworkArn = Lens.lens (\CoreNetworkSummary' {coreNetworkArn} -> coreNetworkArn) (\s@CoreNetworkSummary' {} a -> s {coreNetworkArn = a} :: CoreNetworkSummary)

-- | The ID of a core network.
coreNetworkSummary_coreNetworkId :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_coreNetworkId = Lens.lens (\CoreNetworkSummary' {coreNetworkId} -> coreNetworkId) (\s@CoreNetworkSummary' {} a -> s {coreNetworkId = a} :: CoreNetworkSummary)

-- | The description of a core network.
coreNetworkSummary_description :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_description = Lens.lens (\CoreNetworkSummary' {description} -> description) (\s@CoreNetworkSummary' {} a -> s {description = a} :: CoreNetworkSummary)

-- | The global network ID.
coreNetworkSummary_globalNetworkId :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_globalNetworkId = Lens.lens (\CoreNetworkSummary' {globalNetworkId} -> globalNetworkId) (\s@CoreNetworkSummary' {} a -> s {globalNetworkId = a} :: CoreNetworkSummary)

-- | The ID of the account owner.
coreNetworkSummary_ownerAccountId :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_ownerAccountId = Lens.lens (\CoreNetworkSummary' {ownerAccountId} -> ownerAccountId) (\s@CoreNetworkSummary' {} a -> s {ownerAccountId = a} :: CoreNetworkSummary)

-- | The state of a core network.
coreNetworkSummary_state :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe CoreNetworkState)
coreNetworkSummary_state = Lens.lens (\CoreNetworkSummary' {state} -> state) (\s@CoreNetworkSummary' {} a -> s {state = a} :: CoreNetworkSummary)

-- | The key-value tags associated with a core network summary.
coreNetworkSummary_tags :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe [Tag])
coreNetworkSummary_tags = Lens.lens (\CoreNetworkSummary' {tags} -> tags) (\s@CoreNetworkSummary' {} a -> s {tags = a} :: CoreNetworkSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CoreNetworkSummary where
  parseJSON =
    Data.withObject
      "CoreNetworkSummary"
      ( \x ->
          CoreNetworkSummary'
            Prelude.<$> (x Data..:? "CoreNetworkArn")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CoreNetworkSummary where
  hashWithSalt _salt CoreNetworkSummary' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CoreNetworkSummary where
  rnf CoreNetworkSummary' {..} =
    Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
