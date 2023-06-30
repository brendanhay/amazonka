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
-- Module      : Amazonka.SageMaker.Types.Vertex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Vertex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LineageType

-- | A lineage entity connected to the starting entity(ies).
--
-- /See:/ 'newVertex' smart constructor.
data Vertex = Vertex'
  { -- | The Amazon Resource Name (ARN) of the lineage entity resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of resource of the lineage entity.
    lineageType :: Prelude.Maybe LineageType,
    -- | The type of the lineage entity resource. For example: @DataSet@,
    -- @Model@, @Endpoint@, etc...
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Vertex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'vertex_arn' - The Amazon Resource Name (ARN) of the lineage entity resource.
--
-- 'lineageType', 'vertex_lineageType' - The type of resource of the lineage entity.
--
-- 'type'', 'vertex_type' - The type of the lineage entity resource. For example: @DataSet@,
-- @Model@, @Endpoint@, etc...
newVertex ::
  Vertex
newVertex =
  Vertex'
    { arn = Prelude.Nothing,
      lineageType = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the lineage entity resource.
vertex_arn :: Lens.Lens' Vertex (Prelude.Maybe Prelude.Text)
vertex_arn = Lens.lens (\Vertex' {arn} -> arn) (\s@Vertex' {} a -> s {arn = a} :: Vertex)

-- | The type of resource of the lineage entity.
vertex_lineageType :: Lens.Lens' Vertex (Prelude.Maybe LineageType)
vertex_lineageType = Lens.lens (\Vertex' {lineageType} -> lineageType) (\s@Vertex' {} a -> s {lineageType = a} :: Vertex)

-- | The type of the lineage entity resource. For example: @DataSet@,
-- @Model@, @Endpoint@, etc...
vertex_type :: Lens.Lens' Vertex (Prelude.Maybe Prelude.Text)
vertex_type = Lens.lens (\Vertex' {type'} -> type') (\s@Vertex' {} a -> s {type' = a} :: Vertex)

instance Data.FromJSON Vertex where
  parseJSON =
    Data.withObject
      "Vertex"
      ( \x ->
          Vertex'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "LineageType")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Vertex where
  hashWithSalt _salt Vertex' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lineageType
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Vertex where
  rnf Vertex' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lineageType
      `Prelude.seq` Prelude.rnf type'
