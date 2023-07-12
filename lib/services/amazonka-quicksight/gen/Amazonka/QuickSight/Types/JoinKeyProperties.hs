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
-- Module      : Amazonka.QuickSight.Types.JoinKeyProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.JoinKeyProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties associated with the columns participating in a join.
--
-- /See:/ 'newJoinKeyProperties' smart constructor.
data JoinKeyProperties = JoinKeyProperties'
  { -- | A value that indicates that a row in a table is uniquely identified by
    -- the columns in a join key. This is used by Amazon QuickSight to optimize
    -- query performance.
    uniqueKey :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinKeyProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uniqueKey', 'joinKeyProperties_uniqueKey' - A value that indicates that a row in a table is uniquely identified by
-- the columns in a join key. This is used by Amazon QuickSight to optimize
-- query performance.
newJoinKeyProperties ::
  JoinKeyProperties
newJoinKeyProperties =
  JoinKeyProperties' {uniqueKey = Prelude.Nothing}

-- | A value that indicates that a row in a table is uniquely identified by
-- the columns in a join key. This is used by Amazon QuickSight to optimize
-- query performance.
joinKeyProperties_uniqueKey :: Lens.Lens' JoinKeyProperties (Prelude.Maybe Prelude.Bool)
joinKeyProperties_uniqueKey = Lens.lens (\JoinKeyProperties' {uniqueKey} -> uniqueKey) (\s@JoinKeyProperties' {} a -> s {uniqueKey = a} :: JoinKeyProperties)

instance Data.FromJSON JoinKeyProperties where
  parseJSON =
    Data.withObject
      "JoinKeyProperties"
      ( \x ->
          JoinKeyProperties'
            Prelude.<$> (x Data..:? "UniqueKey")
      )

instance Prelude.Hashable JoinKeyProperties where
  hashWithSalt _salt JoinKeyProperties' {..} =
    _salt `Prelude.hashWithSalt` uniqueKey

instance Prelude.NFData JoinKeyProperties where
  rnf JoinKeyProperties' {..} = Prelude.rnf uniqueKey

instance Data.ToJSON JoinKeyProperties where
  toJSON JoinKeyProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [("UniqueKey" Data..=) Prelude.<$> uniqueKey]
      )
