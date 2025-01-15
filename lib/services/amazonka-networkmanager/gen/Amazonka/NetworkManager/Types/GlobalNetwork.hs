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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.GlobalNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.GlobalNetworkState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a global network. This is a single private network acting as a
-- high-level container for your network objects, including an Amazon Web
-- Services-managed Core Network.
--
-- /See:/ 'newGlobalNetwork' smart constructor.
data GlobalNetwork = GlobalNetwork'
  { -- | The date and time that the global network was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of the global network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the global network.
    globalNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The state of the global network.
    state :: Prelude.Maybe GlobalNetworkState,
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
-- 'createdAt', 'globalNetwork_createdAt' - The date and time that the global network was created.
--
-- 'description', 'globalNetwork_description' - The description of the global network.
--
-- 'globalNetworkArn', 'globalNetwork_globalNetworkArn' - The Amazon Resource Name (ARN) of the global network.
--
-- 'globalNetworkId', 'globalNetwork_globalNetworkId' - The ID of the global network.
--
-- 'state', 'globalNetwork_state' - The state of the global network.
--
-- 'tags', 'globalNetwork_tags' - The tags for the global network.
newGlobalNetwork ::
  GlobalNetwork
newGlobalNetwork =
  GlobalNetwork'
    { createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      globalNetworkArn = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The date and time that the global network was created.
globalNetwork_createdAt :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.UTCTime)
globalNetwork_createdAt = Lens.lens (\GlobalNetwork' {createdAt} -> createdAt) (\s@GlobalNetwork' {} a -> s {createdAt = a} :: GlobalNetwork) Prelude.. Lens.mapping Data._Time

-- | The description of the global network.
globalNetwork_description :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.Text)
globalNetwork_description = Lens.lens (\GlobalNetwork' {description} -> description) (\s@GlobalNetwork' {} a -> s {description = a} :: GlobalNetwork)

-- | The Amazon Resource Name (ARN) of the global network.
globalNetwork_globalNetworkArn :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.Text)
globalNetwork_globalNetworkArn = Lens.lens (\GlobalNetwork' {globalNetworkArn} -> globalNetworkArn) (\s@GlobalNetwork' {} a -> s {globalNetworkArn = a} :: GlobalNetwork)

-- | The ID of the global network.
globalNetwork_globalNetworkId :: Lens.Lens' GlobalNetwork (Prelude.Maybe Prelude.Text)
globalNetwork_globalNetworkId = Lens.lens (\GlobalNetwork' {globalNetworkId} -> globalNetworkId) (\s@GlobalNetwork' {} a -> s {globalNetworkId = a} :: GlobalNetwork)

-- | The state of the global network.
globalNetwork_state :: Lens.Lens' GlobalNetwork (Prelude.Maybe GlobalNetworkState)
globalNetwork_state = Lens.lens (\GlobalNetwork' {state} -> state) (\s@GlobalNetwork' {} a -> s {state = a} :: GlobalNetwork)

-- | The tags for the global network.
globalNetwork_tags :: Lens.Lens' GlobalNetwork (Prelude.Maybe [Tag])
globalNetwork_tags = Lens.lens (\GlobalNetwork' {tags} -> tags) (\s@GlobalNetwork' {} a -> s {tags = a} :: GlobalNetwork) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GlobalNetwork where
  parseJSON =
    Data.withObject
      "GlobalNetwork"
      ( \x ->
          GlobalNetwork'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "GlobalNetworkArn")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GlobalNetwork where
  hashWithSalt _salt GlobalNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalNetworkArn
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData GlobalNetwork where
  rnf GlobalNetwork' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf globalNetworkArn `Prelude.seq`
          Prelude.rnf globalNetworkId `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf tags
