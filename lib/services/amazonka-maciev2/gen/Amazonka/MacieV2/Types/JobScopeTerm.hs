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
-- Module      : Amazonka.MacieV2.Types.JobScopeTerm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.JobScopeTerm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SimpleScopeTerm
import Amazonka.MacieV2.Types.TagScopeTerm
import qualified Amazonka.Prelude as Prelude

-- | Specifies a property- or tag-based condition that defines criteria for
-- including or excluding S3 objects from a classification job. A
-- JobScopeTerm object can contain only one simpleScopeTerm object or one
-- tagScopeTerm object.
--
-- /See:/ 'newJobScopeTerm' smart constructor.
data JobScopeTerm = JobScopeTerm'
  { -- | A tag-based condition that defines the operator and tag keys or tag key
    -- and value pairs for including or excluding objects from the job.
    tagScopeTerm :: Prelude.Maybe TagScopeTerm,
    -- | A property-based condition that defines a property, operator, and one or
    -- more values for including or excluding objects from the job.
    simpleScopeTerm :: Prelude.Maybe SimpleScopeTerm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobScopeTerm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagScopeTerm', 'jobScopeTerm_tagScopeTerm' - A tag-based condition that defines the operator and tag keys or tag key
-- and value pairs for including or excluding objects from the job.
--
-- 'simpleScopeTerm', 'jobScopeTerm_simpleScopeTerm' - A property-based condition that defines a property, operator, and one or
-- more values for including or excluding objects from the job.
newJobScopeTerm ::
  JobScopeTerm
newJobScopeTerm =
  JobScopeTerm'
    { tagScopeTerm = Prelude.Nothing,
      simpleScopeTerm = Prelude.Nothing
    }

-- | A tag-based condition that defines the operator and tag keys or tag key
-- and value pairs for including or excluding objects from the job.
jobScopeTerm_tagScopeTerm :: Lens.Lens' JobScopeTerm (Prelude.Maybe TagScopeTerm)
jobScopeTerm_tagScopeTerm = Lens.lens (\JobScopeTerm' {tagScopeTerm} -> tagScopeTerm) (\s@JobScopeTerm' {} a -> s {tagScopeTerm = a} :: JobScopeTerm)

-- | A property-based condition that defines a property, operator, and one or
-- more values for including or excluding objects from the job.
jobScopeTerm_simpleScopeTerm :: Lens.Lens' JobScopeTerm (Prelude.Maybe SimpleScopeTerm)
jobScopeTerm_simpleScopeTerm = Lens.lens (\JobScopeTerm' {simpleScopeTerm} -> simpleScopeTerm) (\s@JobScopeTerm' {} a -> s {simpleScopeTerm = a} :: JobScopeTerm)

instance Data.FromJSON JobScopeTerm where
  parseJSON =
    Data.withObject
      "JobScopeTerm"
      ( \x ->
          JobScopeTerm'
            Prelude.<$> (x Data..:? "tagScopeTerm")
            Prelude.<*> (x Data..:? "simpleScopeTerm")
      )

instance Prelude.Hashable JobScopeTerm where
  hashWithSalt _salt JobScopeTerm' {..} =
    _salt `Prelude.hashWithSalt` tagScopeTerm
      `Prelude.hashWithSalt` simpleScopeTerm

instance Prelude.NFData JobScopeTerm where
  rnf JobScopeTerm' {..} =
    Prelude.rnf tagScopeTerm
      `Prelude.seq` Prelude.rnf simpleScopeTerm

instance Data.ToJSON JobScopeTerm where
  toJSON JobScopeTerm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tagScopeTerm" Data..=) Prelude.<$> tagScopeTerm,
            ("simpleScopeTerm" Data..=)
              Prelude.<$> simpleScopeTerm
          ]
      )
