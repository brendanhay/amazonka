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
-- Module      : Amazonka.SageMaker.Types.Edge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Edge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AssociationEdgeType

-- | A directed edge connecting two lineage entities.
--
-- /See:/ 'newEdge' smart constructor.
data Edge = Edge'
  { -- | The type of the Association(Edge) between the source and destination.
    -- For example @ContributedTo@, @Produced@, or @DerivedFrom@.
    associationType :: Prelude.Maybe AssociationEdgeType,
    -- | The Amazon Resource Name (ARN) of the destination lineage entity of the
    -- directed edge.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source lineage entity of the
    -- directed edge.
    sourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Edge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationType', 'edge_associationType' - The type of the Association(Edge) between the source and destination.
-- For example @ContributedTo@, @Produced@, or @DerivedFrom@.
--
-- 'destinationArn', 'edge_destinationArn' - The Amazon Resource Name (ARN) of the destination lineage entity of the
-- directed edge.
--
-- 'sourceArn', 'edge_sourceArn' - The Amazon Resource Name (ARN) of the source lineage entity of the
-- directed edge.
newEdge ::
  Edge
newEdge =
  Edge'
    { associationType = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      sourceArn = Prelude.Nothing
    }

-- | The type of the Association(Edge) between the source and destination.
-- For example @ContributedTo@, @Produced@, or @DerivedFrom@.
edge_associationType :: Lens.Lens' Edge (Prelude.Maybe AssociationEdgeType)
edge_associationType = Lens.lens (\Edge' {associationType} -> associationType) (\s@Edge' {} a -> s {associationType = a} :: Edge)

-- | The Amazon Resource Name (ARN) of the destination lineage entity of the
-- directed edge.
edge_destinationArn :: Lens.Lens' Edge (Prelude.Maybe Prelude.Text)
edge_destinationArn = Lens.lens (\Edge' {destinationArn} -> destinationArn) (\s@Edge' {} a -> s {destinationArn = a} :: Edge)

-- | The Amazon Resource Name (ARN) of the source lineage entity of the
-- directed edge.
edge_sourceArn :: Lens.Lens' Edge (Prelude.Maybe Prelude.Text)
edge_sourceArn = Lens.lens (\Edge' {sourceArn} -> sourceArn) (\s@Edge' {} a -> s {sourceArn = a} :: Edge)

instance Data.FromJSON Edge where
  parseJSON =
    Data.withObject
      "Edge"
      ( \x ->
          Edge'
            Prelude.<$> (x Data..:? "AssociationType")
            Prelude.<*> (x Data..:? "DestinationArn")
            Prelude.<*> (x Data..:? "SourceArn")
      )

instance Prelude.Hashable Edge where
  hashWithSalt _salt Edge' {..} =
    _salt
      `Prelude.hashWithSalt` associationType
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData Edge where
  rnf Edge' {..} =
    Prelude.rnf associationType
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf sourceArn
