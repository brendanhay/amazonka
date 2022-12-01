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
-- Module      : Amazonka.NetworkManager.Types.PathComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.PathComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.NetworkResourceSummary
import qualified Amazonka.Prelude as Prelude

-- | Describes a path component.
--
-- /See:/ 'newPathComponent' smart constructor.
data PathComponent = PathComponent'
  { -- | The sequence number in the path. The destination is 0.
    sequence :: Prelude.Maybe Prelude.Int,
    -- | The destination CIDR block in the route table.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The resource.
    resource :: Prelude.Maybe NetworkResourceSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequence', 'pathComponent_sequence' - The sequence number in the path. The destination is 0.
--
-- 'destinationCidrBlock', 'pathComponent_destinationCidrBlock' - The destination CIDR block in the route table.
--
-- 'resource', 'pathComponent_resource' - The resource.
newPathComponent ::
  PathComponent
newPathComponent =
  PathComponent'
    { sequence = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | The sequence number in the path. The destination is 0.
pathComponent_sequence :: Lens.Lens' PathComponent (Prelude.Maybe Prelude.Int)
pathComponent_sequence = Lens.lens (\PathComponent' {sequence} -> sequence) (\s@PathComponent' {} a -> s {sequence = a} :: PathComponent)

-- | The destination CIDR block in the route table.
pathComponent_destinationCidrBlock :: Lens.Lens' PathComponent (Prelude.Maybe Prelude.Text)
pathComponent_destinationCidrBlock = Lens.lens (\PathComponent' {destinationCidrBlock} -> destinationCidrBlock) (\s@PathComponent' {} a -> s {destinationCidrBlock = a} :: PathComponent)

-- | The resource.
pathComponent_resource :: Lens.Lens' PathComponent (Prelude.Maybe NetworkResourceSummary)
pathComponent_resource = Lens.lens (\PathComponent' {resource} -> resource) (\s@PathComponent' {} a -> s {resource = a} :: PathComponent)

instance Core.FromJSON PathComponent where
  parseJSON =
    Core.withObject
      "PathComponent"
      ( \x ->
          PathComponent'
            Prelude.<$> (x Core..:? "Sequence")
            Prelude.<*> (x Core..:? "DestinationCidrBlock")
            Prelude.<*> (x Core..:? "Resource")
      )

instance Prelude.Hashable PathComponent where
  hashWithSalt _salt PathComponent' {..} =
    _salt `Prelude.hashWithSalt` sequence
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` resource

instance Prelude.NFData PathComponent where
  rnf PathComponent' {..} =
    Prelude.rnf sequence
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf resource
