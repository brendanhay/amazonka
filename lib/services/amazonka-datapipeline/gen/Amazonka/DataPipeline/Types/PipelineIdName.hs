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
-- Module      : Amazonka.DataPipeline.Types.PipelineIdName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.PipelineIdName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the name and identifier of a pipeline.
--
-- /See:/ 'newPipelineIdName' smart constructor.
data PipelineIdName = PipelineIdName'
  { -- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a
    -- string of the form @df-297EG78HU43EEXAMPLE@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineIdName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'pipelineIdName_id' - The ID of the pipeline that was assigned by AWS Data Pipeline. This is a
-- string of the form @df-297EG78HU43EEXAMPLE@.
--
-- 'name', 'pipelineIdName_name' - The name of the pipeline.
newPipelineIdName ::
  PipelineIdName
newPipelineIdName =
  PipelineIdName'
    { id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a
-- string of the form @df-297EG78HU43EEXAMPLE@.
pipelineIdName_id :: Lens.Lens' PipelineIdName (Prelude.Maybe Prelude.Text)
pipelineIdName_id = Lens.lens (\PipelineIdName' {id} -> id) (\s@PipelineIdName' {} a -> s {id = a} :: PipelineIdName)

-- | The name of the pipeline.
pipelineIdName_name :: Lens.Lens' PipelineIdName (Prelude.Maybe Prelude.Text)
pipelineIdName_name = Lens.lens (\PipelineIdName' {name} -> name) (\s@PipelineIdName' {} a -> s {name = a} :: PipelineIdName)

instance Data.FromJSON PipelineIdName where
  parseJSON =
    Data.withObject
      "PipelineIdName"
      ( \x ->
          PipelineIdName'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable PipelineIdName where
  hashWithSalt _salt PipelineIdName' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData PipelineIdName where
  rnf PipelineIdName' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
