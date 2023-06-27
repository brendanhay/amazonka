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
-- Module      : Amazonka.MediaConvert.Types.WarningGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WarningGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains any warning codes and their count for the job.
--
-- /See:/ 'newWarningGroup' smart constructor.
data WarningGroup = WarningGroup'
  { -- | The number of times this warning occurred in the job.
    count :: Prelude.Int,
    -- | Warning code that identifies a specific warning in the job. For more
    -- information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/warning_codes.html
    code :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WarningGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'warningGroup_count' - The number of times this warning occurred in the job.
--
-- 'code', 'warningGroup_code' - Warning code that identifies a specific warning in the job. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/warning_codes.html
newWarningGroup ::
  -- | 'count'
  Prelude.Int ->
  -- | 'code'
  Prelude.Int ->
  WarningGroup
newWarningGroup pCount_ pCode_ =
  WarningGroup' {count = pCount_, code = pCode_}

-- | The number of times this warning occurred in the job.
warningGroup_count :: Lens.Lens' WarningGroup Prelude.Int
warningGroup_count = Lens.lens (\WarningGroup' {count} -> count) (\s@WarningGroup' {} a -> s {count = a} :: WarningGroup)

-- | Warning code that identifies a specific warning in the job. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/warning_codes.html
warningGroup_code :: Lens.Lens' WarningGroup Prelude.Int
warningGroup_code = Lens.lens (\WarningGroup' {code} -> code) (\s@WarningGroup' {} a -> s {code = a} :: WarningGroup)

instance Data.FromJSON WarningGroup where
  parseJSON =
    Data.withObject
      "WarningGroup"
      ( \x ->
          WarningGroup'
            Prelude.<$> (x Data..: "count")
            Prelude.<*> (x Data..: "code")
      )

instance Prelude.Hashable WarningGroup where
  hashWithSalt _salt WarningGroup' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` code

instance Prelude.NFData WarningGroup where
  rnf WarningGroup' {..} =
    Prelude.rnf count `Prelude.seq` Prelude.rnf code
