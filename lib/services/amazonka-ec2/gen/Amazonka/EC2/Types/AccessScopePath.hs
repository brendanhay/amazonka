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
-- Module      : Amazonka.EC2.Types.AccessScopePath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AccessScopePath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PathStatement
import Amazonka.EC2.Types.ThroughResourcesStatement
import qualified Amazonka.Prelude as Prelude

-- | Describes a path.
--
-- /See:/ 'newAccessScopePath' smart constructor.
data AccessScopePath = AccessScopePath'
  { -- | The destination.
    destination :: Prelude.Maybe PathStatement,
    -- | The source.
    source :: Prelude.Maybe PathStatement,
    -- | The through resources.
    throughResources :: Prelude.Maybe [ThroughResourcesStatement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessScopePath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'accessScopePath_destination' - The destination.
--
-- 'source', 'accessScopePath_source' - The source.
--
-- 'throughResources', 'accessScopePath_throughResources' - The through resources.
newAccessScopePath ::
  AccessScopePath
newAccessScopePath =
  AccessScopePath'
    { destination = Prelude.Nothing,
      source = Prelude.Nothing,
      throughResources = Prelude.Nothing
    }

-- | The destination.
accessScopePath_destination :: Lens.Lens' AccessScopePath (Prelude.Maybe PathStatement)
accessScopePath_destination = Lens.lens (\AccessScopePath' {destination} -> destination) (\s@AccessScopePath' {} a -> s {destination = a} :: AccessScopePath)

-- | The source.
accessScopePath_source :: Lens.Lens' AccessScopePath (Prelude.Maybe PathStatement)
accessScopePath_source = Lens.lens (\AccessScopePath' {source} -> source) (\s@AccessScopePath' {} a -> s {source = a} :: AccessScopePath)

-- | The through resources.
accessScopePath_throughResources :: Lens.Lens' AccessScopePath (Prelude.Maybe [ThroughResourcesStatement])
accessScopePath_throughResources = Lens.lens (\AccessScopePath' {throughResources} -> throughResources) (\s@AccessScopePath' {} a -> s {throughResources = a} :: AccessScopePath) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML AccessScopePath where
  parseXML x =
    AccessScopePath'
      Prelude.<$> (x Data..@? "destination")
      Prelude.<*> (x Data..@? "source")
      Prelude.<*> ( x
                      Data..@? "throughResourceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable AccessScopePath where
  hashWithSalt _salt AccessScopePath' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` throughResources

instance Prelude.NFData AccessScopePath where
  rnf AccessScopePath' {..} =
    Prelude.rnf destination `Prelude.seq`
      Prelude.rnf source `Prelude.seq`
        Prelude.rnf throughResources
