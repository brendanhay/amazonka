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
-- Module      : Amazonka.OsIs.Types.PipelineStatusReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.PipelineStatusReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a pipeline\'s current status.
--
-- /See:/ 'newPipelineStatusReason' smart constructor.
data PipelineStatusReason = PipelineStatusReason'
  { -- | A description of why a pipeline has a certain status.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineStatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'pipelineStatusReason_description' - A description of why a pipeline has a certain status.
newPipelineStatusReason ::
  PipelineStatusReason
newPipelineStatusReason =
  PipelineStatusReason'
    { description =
        Prelude.Nothing
    }

-- | A description of why a pipeline has a certain status.
pipelineStatusReason_description :: Lens.Lens' PipelineStatusReason (Prelude.Maybe Prelude.Text)
pipelineStatusReason_description = Lens.lens (\PipelineStatusReason' {description} -> description) (\s@PipelineStatusReason' {} a -> s {description = a} :: PipelineStatusReason)

instance Data.FromJSON PipelineStatusReason where
  parseJSON =
    Data.withObject
      "PipelineStatusReason"
      ( \x ->
          PipelineStatusReason'
            Prelude.<$> (x Data..:? "Description")
      )

instance Prelude.Hashable PipelineStatusReason where
  hashWithSalt _salt PipelineStatusReason' {..} =
    _salt `Prelude.hashWithSalt` description

instance Prelude.NFData PipelineStatusReason where
  rnf PipelineStatusReason' {..} =
    Prelude.rnf description
