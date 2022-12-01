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
-- Module      : Amazonka.M2.Types.BatchJobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.BatchJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types.FileBatchJobDefinition
import Amazonka.M2.Types.ScriptBatchJobDefinition
import qualified Amazonka.Prelude as Prelude

-- | Defines the details of a batch job.
--
-- /See:/ 'newBatchJobDefinition' smart constructor.
data BatchJobDefinition = BatchJobDefinition'
  { -- | Specifies a file containing a batch job definition.
    fileBatchJobDefinition :: Prelude.Maybe FileBatchJobDefinition,
    -- | A script containing a batch job definition.
    scriptBatchJobDefinition :: Prelude.Maybe ScriptBatchJobDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileBatchJobDefinition', 'batchJobDefinition_fileBatchJobDefinition' - Specifies a file containing a batch job definition.
--
-- 'scriptBatchJobDefinition', 'batchJobDefinition_scriptBatchJobDefinition' - A script containing a batch job definition.
newBatchJobDefinition ::
  BatchJobDefinition
newBatchJobDefinition =
  BatchJobDefinition'
    { fileBatchJobDefinition =
        Prelude.Nothing,
      scriptBatchJobDefinition = Prelude.Nothing
    }

-- | Specifies a file containing a batch job definition.
batchJobDefinition_fileBatchJobDefinition :: Lens.Lens' BatchJobDefinition (Prelude.Maybe FileBatchJobDefinition)
batchJobDefinition_fileBatchJobDefinition = Lens.lens (\BatchJobDefinition' {fileBatchJobDefinition} -> fileBatchJobDefinition) (\s@BatchJobDefinition' {} a -> s {fileBatchJobDefinition = a} :: BatchJobDefinition)

-- | A script containing a batch job definition.
batchJobDefinition_scriptBatchJobDefinition :: Lens.Lens' BatchJobDefinition (Prelude.Maybe ScriptBatchJobDefinition)
batchJobDefinition_scriptBatchJobDefinition = Lens.lens (\BatchJobDefinition' {scriptBatchJobDefinition} -> scriptBatchJobDefinition) (\s@BatchJobDefinition' {} a -> s {scriptBatchJobDefinition = a} :: BatchJobDefinition)

instance Core.FromJSON BatchJobDefinition where
  parseJSON =
    Core.withObject
      "BatchJobDefinition"
      ( \x ->
          BatchJobDefinition'
            Prelude.<$> (x Core..:? "fileBatchJobDefinition")
            Prelude.<*> (x Core..:? "scriptBatchJobDefinition")
      )

instance Prelude.Hashable BatchJobDefinition where
  hashWithSalt _salt BatchJobDefinition' {..} =
    _salt `Prelude.hashWithSalt` fileBatchJobDefinition
      `Prelude.hashWithSalt` scriptBatchJobDefinition

instance Prelude.NFData BatchJobDefinition where
  rnf BatchJobDefinition' {..} =
    Prelude.rnf fileBatchJobDefinition
      `Prelude.seq` Prelude.rnf scriptBatchJobDefinition
