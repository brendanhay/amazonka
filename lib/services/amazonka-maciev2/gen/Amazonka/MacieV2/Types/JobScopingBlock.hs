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
-- Module      : Amazonka.MacieV2.Types.JobScopingBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.JobScopingBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.JobScopeTerm
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more property- and tag-based conditions that define
-- criteria for including or excluding S3 objects from a classification
-- job.
--
-- /See:/ 'newJobScopingBlock' smart constructor.
data JobScopingBlock = JobScopingBlock'
  { -- | An array of conditions, one for each property- or tag-based condition
    -- that determines which objects to include or exclude from the job. If you
    -- specify more than one condition, Amazon Macie uses AND logic to join the
    -- conditions.
    and :: Prelude.Maybe [JobScopeTerm]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobScopingBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'and', 'jobScopingBlock_and' - An array of conditions, one for each property- or tag-based condition
-- that determines which objects to include or exclude from the job. If you
-- specify more than one condition, Amazon Macie uses AND logic to join the
-- conditions.
newJobScopingBlock ::
  JobScopingBlock
newJobScopingBlock =
  JobScopingBlock' {and = Prelude.Nothing}

-- | An array of conditions, one for each property- or tag-based condition
-- that determines which objects to include or exclude from the job. If you
-- specify more than one condition, Amazon Macie uses AND logic to join the
-- conditions.
jobScopingBlock_and :: Lens.Lens' JobScopingBlock (Prelude.Maybe [JobScopeTerm])
jobScopingBlock_and = Lens.lens (\JobScopingBlock' {and} -> and) (\s@JobScopingBlock' {} a -> s {and = a} :: JobScopingBlock) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobScopingBlock where
  parseJSON =
    Data.withObject
      "JobScopingBlock"
      ( \x ->
          JobScopingBlock'
            Prelude.<$> (x Data..:? "and" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JobScopingBlock where
  hashWithSalt _salt JobScopingBlock' {..} =
    _salt `Prelude.hashWithSalt` and

instance Prelude.NFData JobScopingBlock where
  rnf JobScopingBlock' {..} = Prelude.rnf and

instance Data.ToJSON JobScopingBlock where
  toJSON JobScopingBlock' {..} =
    Data.object
      (Prelude.catMaybes [("and" Data..=) Prelude.<$> and])
