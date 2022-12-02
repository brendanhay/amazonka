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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The global network ID.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The key-value tags associated with a core network summary.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The state of a core network.
    state :: Prelude.Maybe CoreNetworkState,
    -- | The description of a core network.
    description :: Prelude.Maybe Prelude.Text,
    -- | a core network ARN.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text
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
-- 'globalNetworkId', 'coreNetworkSummary_globalNetworkId' - The global network ID.
--
-- 'tags', 'coreNetworkSummary_tags' - The key-value tags associated with a core network summary.
--
-- 'coreNetworkId', 'coreNetworkSummary_coreNetworkId' - The ID of a core network.
--
-- 'state', 'coreNetworkSummary_state' - The state of a core network.
--
-- 'description', 'coreNetworkSummary_description' - The description of a core network.
--
-- 'coreNetworkArn', 'coreNetworkSummary_coreNetworkArn' - a core network ARN.
--
-- 'ownerAccountId', 'coreNetworkSummary_ownerAccountId' - The ID of the account owner.
newCoreNetworkSummary ::
  CoreNetworkSummary
newCoreNetworkSummary =
  CoreNetworkSummary'
    { globalNetworkId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      coreNetworkArn = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing
    }

-- | The global network ID.
coreNetworkSummary_globalNetworkId :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_globalNetworkId = Lens.lens (\CoreNetworkSummary' {globalNetworkId} -> globalNetworkId) (\s@CoreNetworkSummary' {} a -> s {globalNetworkId = a} :: CoreNetworkSummary)

-- | The key-value tags associated with a core network summary.
coreNetworkSummary_tags :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe [Tag])
coreNetworkSummary_tags = Lens.lens (\CoreNetworkSummary' {tags} -> tags) (\s@CoreNetworkSummary' {} a -> s {tags = a} :: CoreNetworkSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a core network.
coreNetworkSummary_coreNetworkId :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_coreNetworkId = Lens.lens (\CoreNetworkSummary' {coreNetworkId} -> coreNetworkId) (\s@CoreNetworkSummary' {} a -> s {coreNetworkId = a} :: CoreNetworkSummary)

-- | The state of a core network.
coreNetworkSummary_state :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe CoreNetworkState)
coreNetworkSummary_state = Lens.lens (\CoreNetworkSummary' {state} -> state) (\s@CoreNetworkSummary' {} a -> s {state = a} :: CoreNetworkSummary)

-- | The description of a core network.
coreNetworkSummary_description :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_description = Lens.lens (\CoreNetworkSummary' {description} -> description) (\s@CoreNetworkSummary' {} a -> s {description = a} :: CoreNetworkSummary)

-- | a core network ARN.
coreNetworkSummary_coreNetworkArn :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_coreNetworkArn = Lens.lens (\CoreNetworkSummary' {coreNetworkArn} -> coreNetworkArn) (\s@CoreNetworkSummary' {} a -> s {coreNetworkArn = a} :: CoreNetworkSummary)

-- | The ID of the account owner.
coreNetworkSummary_ownerAccountId :: Lens.Lens' CoreNetworkSummary (Prelude.Maybe Prelude.Text)
coreNetworkSummary_ownerAccountId = Lens.lens (\CoreNetworkSummary' {ownerAccountId} -> ownerAccountId) (\s@CoreNetworkSummary' {} a -> s {ownerAccountId = a} :: CoreNetworkSummary)

instance Data.FromJSON CoreNetworkSummary where
  parseJSON =
    Data.withObject
      "CoreNetworkSummary"
      ( \x ->
          CoreNetworkSummary'
            Prelude.<$> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CoreNetworkArn")
            Prelude.<*> (x Data..:? "OwnerAccountId")
      )

instance Prelude.Hashable CoreNetworkSummary where
  hashWithSalt _salt CoreNetworkSummary' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` ownerAccountId

instance Prelude.NFData CoreNetworkSummary where
  rnf CoreNetworkSummary' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf ownerAccountId
