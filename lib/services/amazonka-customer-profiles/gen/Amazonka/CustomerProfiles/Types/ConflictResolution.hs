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
-- Module      : Amazonka.CustomerProfiles.Types.ConflictResolution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ConflictResolution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.ConflictResolvingModel
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | How the auto-merging process should resolve conflicts between different
-- profiles.
--
-- /See:/ 'newConflictResolution' smart constructor.
data ConflictResolution = ConflictResolution'
  { -- | The @ObjectType@ name that is used to resolve profile merging conflicts
    -- when choosing @SOURCE@ as the @ConflictResolvingModel@.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | How the auto-merging process should resolve conflicts between different
    -- profiles.
    --
    -- -   @RECENCY@: Uses the data that was most recently updated.
    --
    -- -   @SOURCE@: Uses the data from a specific source. For example, if a
    --     company has been aquired or two departments have merged, data from
    --     the specified source is used. If two duplicate profiles are from the
    --     same source, then @RECENCY@ is used again.
    conflictResolvingModel :: ConflictResolvingModel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConflictResolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceName', 'conflictResolution_sourceName' - The @ObjectType@ name that is used to resolve profile merging conflicts
-- when choosing @SOURCE@ as the @ConflictResolvingModel@.
--
-- 'conflictResolvingModel', 'conflictResolution_conflictResolvingModel' - How the auto-merging process should resolve conflicts between different
-- profiles.
--
-- -   @RECENCY@: Uses the data that was most recently updated.
--
-- -   @SOURCE@: Uses the data from a specific source. For example, if a
--     company has been aquired or two departments have merged, data from
--     the specified source is used. If two duplicate profiles are from the
--     same source, then @RECENCY@ is used again.
newConflictResolution ::
  -- | 'conflictResolvingModel'
  ConflictResolvingModel ->
  ConflictResolution
newConflictResolution pConflictResolvingModel_ =
  ConflictResolution'
    { sourceName = Prelude.Nothing,
      conflictResolvingModel = pConflictResolvingModel_
    }

-- | The @ObjectType@ name that is used to resolve profile merging conflicts
-- when choosing @SOURCE@ as the @ConflictResolvingModel@.
conflictResolution_sourceName :: Lens.Lens' ConflictResolution (Prelude.Maybe Prelude.Text)
conflictResolution_sourceName = Lens.lens (\ConflictResolution' {sourceName} -> sourceName) (\s@ConflictResolution' {} a -> s {sourceName = a} :: ConflictResolution)

-- | How the auto-merging process should resolve conflicts between different
-- profiles.
--
-- -   @RECENCY@: Uses the data that was most recently updated.
--
-- -   @SOURCE@: Uses the data from a specific source. For example, if a
--     company has been aquired or two departments have merged, data from
--     the specified source is used. If two duplicate profiles are from the
--     same source, then @RECENCY@ is used again.
conflictResolution_conflictResolvingModel :: Lens.Lens' ConflictResolution ConflictResolvingModel
conflictResolution_conflictResolvingModel = Lens.lens (\ConflictResolution' {conflictResolvingModel} -> conflictResolvingModel) (\s@ConflictResolution' {} a -> s {conflictResolvingModel = a} :: ConflictResolution)

instance Data.FromJSON ConflictResolution where
  parseJSON =
    Data.withObject
      "ConflictResolution"
      ( \x ->
          ConflictResolution'
            Prelude.<$> (x Data..:? "SourceName")
            Prelude.<*> (x Data..: "ConflictResolvingModel")
      )

instance Prelude.Hashable ConflictResolution where
  hashWithSalt _salt ConflictResolution' {..} =
    _salt `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` conflictResolvingModel

instance Prelude.NFData ConflictResolution where
  rnf ConflictResolution' {..} =
    Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf conflictResolvingModel

instance Data.ToJSON ConflictResolution where
  toJSON ConflictResolution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceName" Data..=) Prelude.<$> sourceName,
            Prelude.Just
              ( "ConflictResolvingModel"
                  Data..= conflictResolvingModel
              )
          ]
      )
