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
-- Module      : Amazonka.CodeBuild.Types.BatchRestrictions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BatchRestrictions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies restrictions for the batch build.
--
-- /See:/ 'newBatchRestrictions' smart constructor.
data BatchRestrictions = BatchRestrictions'
  { -- | Specifies the maximum number of builds allowed.
    maximumBuildsAllowed :: Prelude.Maybe Prelude.Int,
    -- | An array of strings that specify the compute types that are allowed for
    -- the batch build. See
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
    -- in the /CodeBuild User Guide/ for these values.
    computeTypesAllowed :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchRestrictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumBuildsAllowed', 'batchRestrictions_maximumBuildsAllowed' - Specifies the maximum number of builds allowed.
--
-- 'computeTypesAllowed', 'batchRestrictions_computeTypesAllowed' - An array of strings that specify the compute types that are allowed for
-- the batch build. See
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
-- in the /CodeBuild User Guide/ for these values.
newBatchRestrictions ::
  BatchRestrictions
newBatchRestrictions =
  BatchRestrictions'
    { maximumBuildsAllowed =
        Prelude.Nothing,
      computeTypesAllowed = Prelude.Nothing
    }

-- | Specifies the maximum number of builds allowed.
batchRestrictions_maximumBuildsAllowed :: Lens.Lens' BatchRestrictions (Prelude.Maybe Prelude.Int)
batchRestrictions_maximumBuildsAllowed = Lens.lens (\BatchRestrictions' {maximumBuildsAllowed} -> maximumBuildsAllowed) (\s@BatchRestrictions' {} a -> s {maximumBuildsAllowed = a} :: BatchRestrictions)

-- | An array of strings that specify the compute types that are allowed for
-- the batch build. See
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
-- in the /CodeBuild User Guide/ for these values.
batchRestrictions_computeTypesAllowed :: Lens.Lens' BatchRestrictions (Prelude.Maybe [Prelude.Text])
batchRestrictions_computeTypesAllowed = Lens.lens (\BatchRestrictions' {computeTypesAllowed} -> computeTypesAllowed) (\s@BatchRestrictions' {} a -> s {computeTypesAllowed = a} :: BatchRestrictions) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON BatchRestrictions where
  parseJSON =
    Core.withObject
      "BatchRestrictions"
      ( \x ->
          BatchRestrictions'
            Prelude.<$> (x Core..:? "maximumBuildsAllowed")
            Prelude.<*> ( x Core..:? "computeTypesAllowed"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchRestrictions

instance Prelude.NFData BatchRestrictions

instance Core.ToJSON BatchRestrictions where
  toJSON BatchRestrictions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maximumBuildsAllowed" Core..=)
              Prelude.<$> maximumBuildsAllowed,
            ("computeTypesAllowed" Core..=)
              Prelude.<$> computeTypesAllowed
          ]
      )
