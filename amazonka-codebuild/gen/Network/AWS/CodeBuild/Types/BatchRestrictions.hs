{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.BatchRestrictions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BatchRestrictions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies restrictions for the batch build.
--
-- /See:/ 'newBatchRestrictions' smart constructor.
data BatchRestrictions = BatchRestrictions'
  { -- | An array of strings that specify the compute types that are allowed for
    -- the batch build. See
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
    -- in the /AWS CodeBuild User Guide/ for these values.
    computeTypesAllowed :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the maximum number of builds allowed.
    maximumBuildsAllowed :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchRestrictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeTypesAllowed', 'batchRestrictions_computeTypesAllowed' - An array of strings that specify the compute types that are allowed for
-- the batch build. See
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
-- in the /AWS CodeBuild User Guide/ for these values.
--
-- 'maximumBuildsAllowed', 'batchRestrictions_maximumBuildsAllowed' - Specifies the maximum number of builds allowed.
newBatchRestrictions ::
  BatchRestrictions
newBatchRestrictions =
  BatchRestrictions'
    { computeTypesAllowed =
        Prelude.Nothing,
      maximumBuildsAllowed = Prelude.Nothing
    }

-- | An array of strings that specify the compute types that are allowed for
-- the batch build. See
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types>
-- in the /AWS CodeBuild User Guide/ for these values.
batchRestrictions_computeTypesAllowed :: Lens.Lens' BatchRestrictions (Prelude.Maybe [Prelude.Text])
batchRestrictions_computeTypesAllowed = Lens.lens (\BatchRestrictions' {computeTypesAllowed} -> computeTypesAllowed) (\s@BatchRestrictions' {} a -> s {computeTypesAllowed = a} :: BatchRestrictions) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the maximum number of builds allowed.
batchRestrictions_maximumBuildsAllowed :: Lens.Lens' BatchRestrictions (Prelude.Maybe Prelude.Int)
batchRestrictions_maximumBuildsAllowed = Lens.lens (\BatchRestrictions' {maximumBuildsAllowed} -> maximumBuildsAllowed) (\s@BatchRestrictions' {} a -> s {maximumBuildsAllowed = a} :: BatchRestrictions)

instance Prelude.FromJSON BatchRestrictions where
  parseJSON =
    Prelude.withObject
      "BatchRestrictions"
      ( \x ->
          BatchRestrictions'
            Prelude.<$> ( x Prelude..:? "computeTypesAllowed"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "maximumBuildsAllowed")
      )

instance Prelude.Hashable BatchRestrictions

instance Prelude.NFData BatchRestrictions

instance Prelude.ToJSON BatchRestrictions where
  toJSON BatchRestrictions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("computeTypesAllowed" Prelude..=)
              Prelude.<$> computeTypesAllowed,
            ("maximumBuildsAllowed" Prelude..=)
              Prelude.<$> maximumBuildsAllowed
          ]
      )
