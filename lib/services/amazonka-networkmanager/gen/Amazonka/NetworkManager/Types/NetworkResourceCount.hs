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
-- Module      : Amazonka.NetworkManager.Types.NetworkResourceCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.NetworkResourceCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource count.
--
-- /See:/ 'newNetworkResourceCount' smart constructor.
data NetworkResourceCount = NetworkResourceCount'
  { -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The resource count.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkResourceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'networkResourceCount_resourceType' - The resource type.
--
-- 'count', 'networkResourceCount_count' - The resource count.
newNetworkResourceCount ::
  NetworkResourceCount
newNetworkResourceCount =
  NetworkResourceCount'
    { resourceType =
        Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The resource type.
networkResourceCount_resourceType :: Lens.Lens' NetworkResourceCount (Prelude.Maybe Prelude.Text)
networkResourceCount_resourceType = Lens.lens (\NetworkResourceCount' {resourceType} -> resourceType) (\s@NetworkResourceCount' {} a -> s {resourceType = a} :: NetworkResourceCount)

-- | The resource count.
networkResourceCount_count :: Lens.Lens' NetworkResourceCount (Prelude.Maybe Prelude.Int)
networkResourceCount_count = Lens.lens (\NetworkResourceCount' {count} -> count) (\s@NetworkResourceCount' {} a -> s {count = a} :: NetworkResourceCount)

instance Data.FromJSON NetworkResourceCount where
  parseJSON =
    Data.withObject
      "NetworkResourceCount"
      ( \x ->
          NetworkResourceCount'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Count")
      )

instance Prelude.Hashable NetworkResourceCount where
  hashWithSalt _salt NetworkResourceCount' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` count

instance Prelude.NFData NetworkResourceCount where
  rnf NetworkResourceCount' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf count
