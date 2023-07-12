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
-- Module      : Amazonka.M2.Types.RecordLength
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.RecordLength where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The length of the records in the data set.
--
-- /See:/ 'newRecordLength' smart constructor.
data RecordLength = RecordLength'
  { -- | The maximum record length. In case of fixed, both minimum and maximum
    -- are the same.
    max :: Prelude.Int,
    -- | The minimum record length of a record.
    min :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordLength' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'recordLength_max' - The maximum record length. In case of fixed, both minimum and maximum
-- are the same.
--
-- 'min', 'recordLength_min' - The minimum record length of a record.
newRecordLength ::
  -- | 'max'
  Prelude.Int ->
  -- | 'min'
  Prelude.Int ->
  RecordLength
newRecordLength pMax_ pMin_ =
  RecordLength' {max = pMax_, min = pMin_}

-- | The maximum record length. In case of fixed, both minimum and maximum
-- are the same.
recordLength_max :: Lens.Lens' RecordLength Prelude.Int
recordLength_max = Lens.lens (\RecordLength' {max} -> max) (\s@RecordLength' {} a -> s {max = a} :: RecordLength)

-- | The minimum record length of a record.
recordLength_min :: Lens.Lens' RecordLength Prelude.Int
recordLength_min = Lens.lens (\RecordLength' {min} -> min) (\s@RecordLength' {} a -> s {min = a} :: RecordLength)

instance Prelude.Hashable RecordLength where
  hashWithSalt _salt RecordLength' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData RecordLength where
  rnf RecordLength' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToJSON RecordLength where
  toJSON RecordLength' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("max" Data..= max),
            Prelude.Just ("min" Data..= min)
          ]
      )
