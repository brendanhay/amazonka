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
-- Module      : Amazonka.DataPipeline.Types.PipelineObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.PipelineObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types.Field
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a pipeline object. This can be a logical,
-- physical, or physical attempt pipeline object. The complete set of
-- components of a pipeline defines the pipeline.
--
-- /See:/ 'newPipelineObject' smart constructor.
data PipelineObject = PipelineObject'
  { -- | The ID of the object.
    id :: Prelude.Text,
    -- | The name of the object.
    name :: Prelude.Text,
    -- | Key-value pairs that define the properties of the object.
    fields :: [Field]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'pipelineObject_id' - The ID of the object.
--
-- 'name', 'pipelineObject_name' - The name of the object.
--
-- 'fields', 'pipelineObject_fields' - Key-value pairs that define the properties of the object.
newPipelineObject ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  PipelineObject
newPipelineObject pId_ pName_ =
  PipelineObject'
    { id = pId_,
      name = pName_,
      fields = Prelude.mempty
    }

-- | The ID of the object.
pipelineObject_id :: Lens.Lens' PipelineObject Prelude.Text
pipelineObject_id = Lens.lens (\PipelineObject' {id} -> id) (\s@PipelineObject' {} a -> s {id = a} :: PipelineObject)

-- | The name of the object.
pipelineObject_name :: Lens.Lens' PipelineObject Prelude.Text
pipelineObject_name = Lens.lens (\PipelineObject' {name} -> name) (\s@PipelineObject' {} a -> s {name = a} :: PipelineObject)

-- | Key-value pairs that define the properties of the object.
pipelineObject_fields :: Lens.Lens' PipelineObject [Field]
pipelineObject_fields = Lens.lens (\PipelineObject' {fields} -> fields) (\s@PipelineObject' {} a -> s {fields = a} :: PipelineObject) Prelude.. Lens.coerced

instance Core.FromJSON PipelineObject where
  parseJSON =
    Core.withObject
      "PipelineObject"
      ( \x ->
          PipelineObject'
            Prelude.<$> (x Core..: "id")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..:? "fields" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable PipelineObject where
  hashWithSalt _salt PipelineObject' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fields

instance Prelude.NFData PipelineObject where
  rnf PipelineObject' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf fields

instance Core.ToJSON PipelineObject where
  toJSON PipelineObject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Core..= id),
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("fields" Core..= fields)
          ]
      )
