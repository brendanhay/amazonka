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
-- Module      : Amazonka.NetworkManager.Types.GlobalNetwork
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.GlobalNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkManager.Types.GlobalNetworkState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a global network.
--
-- /See:/ 'newGlobalNetwork' smart constructor.
data GlobalNetwork = GlobalNetwork'
  { -- | The state of the global network.
    state :: Prelude.Maybe GlobalNetworkState,
    -- | The date and time that the global network was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the global network.
    globalNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The description of the global network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the global network.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'globalNetwork_state' - The state of the global network.
--
-- 'createdAt', 'globalNetwork_createdAt' - The date and time that the global network was created.
--
-- 'globalNetworkArn', 'globalNetwork_globalNetworkArn' - The Amazon Resource Name (ARN) of the global network.
--
-- 'globalNetworkId', 'globalNetwork_globalNetworkId' - The ID of the global network.
--
-- 'description', 'globalNetwork_description' - The description of the global network.
--
-- 'tags', 'globalNetwork_tags' - The tags for the global network.
newGlobalNetwork ::
  GlobalNetwork
newGlobalNetwork =
  GlobalNetwork'
    { state = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      globalNetworkArn = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The state of the global network.
globalNetwork_state :: Lens.Lens' GlobalNetwork (Prelude.Maybe GlobalNetworkState)
globalNetwork_state = Lens.lens (\GlobalNetwork' {state} -> state) (\s@GlobalNetwork' {} a -> s {state = a} :: GlobalNetwork)

-- | The date and time that the global network was created.
globalNetwork_createdAt :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.UTCTime)
globalNetwork_createdAt = Lens.lens (\GlobalNetwork' {createdAt} -> createdAt) (\s@GlobalNetwork' {} a -> s {createdAt = a} :: GlobalNetwork) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the global network.
globalNetwork_globalNetworkArn :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.Text)
globalNetwork_globalNetworkArn = Lens.lens (\GlobalNetwork' {globalNetworkArn} -> globalNetworkArn) (\s@GlobalNetwork' {} a -> s {globalNetworkArn = a} :: GlobalNetwork)

-- | The ID of the global network.
globalNetwork_globalNetworkId :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.Text)
globalNetwork_globalNetworkId = Lens.lens (\GlobalNetwork' {globalNetworkId} -> globalNetworkId) (\s@GlobalNetwork' {} a -> s {globalNetworkId = a} :: GlobalNetwork)

-- | The description of the global network.
globalNetwork_description :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.Text)
globalNetwork_description = Lens.lens (\GlobalNetwork' {description} -> description) (\s@GlobalNetwork' {} a -> s {description = a} :: GlobalNetwork)

-- | The tags for the global network.
globalNetwork_tags :: Lens.Lens' GlobalNetwork (Prelude.Maybe [Tag])
globalNetwork_tags = Lens.lens (\GlobalNetwork' {tags} -> tags) (\s@GlobalNetwork' {} a -> s {tags = a} :: GlobalNetwork) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON GlobalNetwork where
  parseJSON =
    Core.withObject
      "GlobalNetwork"
      ( \x ->
          GlobalNetwork'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "GlobalNetworkArn")
            Prelude.<*> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable GlobalNetwork where
  hashWithSalt _salt GlobalNetwork' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` globalNetworkArn
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags

instance Prelude.NFData GlobalNetwork where
  rnf GlobalNetwork' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf globalNetworkArn
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
