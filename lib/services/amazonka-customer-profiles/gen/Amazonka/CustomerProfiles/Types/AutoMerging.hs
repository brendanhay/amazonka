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
-- Module      : Amazonka.CustomerProfiles.Types.AutoMerging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AutoMerging where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.ConflictResolution
import Amazonka.CustomerProfiles.Types.Consolidation
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for how to perform the auto-merging of profiles.
--
-- /See:/ 'newAutoMerging' smart constructor.
data AutoMerging = AutoMerging'
  { -- | How the auto-merging process should resolve conflicts between different
    -- profiles. For example, if Profile A and Profile B have the same
    -- @FirstName@ and @LastName@ (and that is the matching criteria), which
    -- @EmailAddress@ should be used?
    conflictResolution :: Prelude.Maybe ConflictResolution,
    -- | A list of matching attributes that represent matching criteria. If two
    -- profiles meet at least one of the requirements in the matching
    -- attributes list, they will be merged.
    consolidation :: Prelude.Maybe Consolidation,
    -- | A number between 0 and 1 that represents the minimum confidence score
    -- required for profiles within a matching group to be merged during the
    -- auto-merge process. A higher score means higher similarity required to
    -- merge profiles.
    minAllowedConfidenceScoreForMerging :: Prelude.Maybe Prelude.Double,
    -- | The flag that enables the auto-merging of duplicate profiles.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMerging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictResolution', 'autoMerging_conflictResolution' - How the auto-merging process should resolve conflicts between different
-- profiles. For example, if Profile A and Profile B have the same
-- @FirstName@ and @LastName@ (and that is the matching criteria), which
-- @EmailAddress@ should be used?
--
-- 'consolidation', 'autoMerging_consolidation' - A list of matching attributes that represent matching criteria. If two
-- profiles meet at least one of the requirements in the matching
-- attributes list, they will be merged.
--
-- 'minAllowedConfidenceScoreForMerging', 'autoMerging_minAllowedConfidenceScoreForMerging' - A number between 0 and 1 that represents the minimum confidence score
-- required for profiles within a matching group to be merged during the
-- auto-merge process. A higher score means higher similarity required to
-- merge profiles.
--
-- 'enabled', 'autoMerging_enabled' - The flag that enables the auto-merging of duplicate profiles.
newAutoMerging ::
  -- | 'enabled'
  Prelude.Bool ->
  AutoMerging
newAutoMerging pEnabled_ =
  AutoMerging'
    { conflictResolution = Prelude.Nothing,
      consolidation = Prelude.Nothing,
      minAllowedConfidenceScoreForMerging =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | How the auto-merging process should resolve conflicts between different
-- profiles. For example, if Profile A and Profile B have the same
-- @FirstName@ and @LastName@ (and that is the matching criteria), which
-- @EmailAddress@ should be used?
autoMerging_conflictResolution :: Lens.Lens' AutoMerging (Prelude.Maybe ConflictResolution)
autoMerging_conflictResolution = Lens.lens (\AutoMerging' {conflictResolution} -> conflictResolution) (\s@AutoMerging' {} a -> s {conflictResolution = a} :: AutoMerging)

-- | A list of matching attributes that represent matching criteria. If two
-- profiles meet at least one of the requirements in the matching
-- attributes list, they will be merged.
autoMerging_consolidation :: Lens.Lens' AutoMerging (Prelude.Maybe Consolidation)
autoMerging_consolidation = Lens.lens (\AutoMerging' {consolidation} -> consolidation) (\s@AutoMerging' {} a -> s {consolidation = a} :: AutoMerging)

-- | A number between 0 and 1 that represents the minimum confidence score
-- required for profiles within a matching group to be merged during the
-- auto-merge process. A higher score means higher similarity required to
-- merge profiles.
autoMerging_minAllowedConfidenceScoreForMerging :: Lens.Lens' AutoMerging (Prelude.Maybe Prelude.Double)
autoMerging_minAllowedConfidenceScoreForMerging = Lens.lens (\AutoMerging' {minAllowedConfidenceScoreForMerging} -> minAllowedConfidenceScoreForMerging) (\s@AutoMerging' {} a -> s {minAllowedConfidenceScoreForMerging = a} :: AutoMerging)

-- | The flag that enables the auto-merging of duplicate profiles.
autoMerging_enabled :: Lens.Lens' AutoMerging Prelude.Bool
autoMerging_enabled = Lens.lens (\AutoMerging' {enabled} -> enabled) (\s@AutoMerging' {} a -> s {enabled = a} :: AutoMerging)

instance Data.FromJSON AutoMerging where
  parseJSON =
    Data.withObject
      "AutoMerging"
      ( \x ->
          AutoMerging'
            Prelude.<$> (x Data..:? "ConflictResolution")
            Prelude.<*> (x Data..:? "Consolidation")
            Prelude.<*> (x Data..:? "MinAllowedConfidenceScoreForMerging")
            Prelude.<*> (x Data..: "Enabled")
      )

instance Prelude.Hashable AutoMerging where
  hashWithSalt _salt AutoMerging' {..} =
    _salt
      `Prelude.hashWithSalt` conflictResolution
      `Prelude.hashWithSalt` consolidation
      `Prelude.hashWithSalt` minAllowedConfidenceScoreForMerging
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData AutoMerging where
  rnf AutoMerging' {..} =
    Prelude.rnf conflictResolution `Prelude.seq`
      Prelude.rnf consolidation `Prelude.seq`
        Prelude.rnf minAllowedConfidenceScoreForMerging `Prelude.seq`
          Prelude.rnf enabled

instance Data.ToJSON AutoMerging where
  toJSON AutoMerging' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConflictResolution" Data..=)
              Prelude.<$> conflictResolution,
            ("Consolidation" Data..=) Prelude.<$> consolidation,
            ("MinAllowedConfidenceScoreForMerging" Data..=)
              Prelude.<$> minAllowedConfidenceScoreForMerging,
            Prelude.Just ("Enabled" Data..= enabled)
          ]
      )
