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
-- Module      : Amazonka.LicenseManager.Types.BorrowConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.BorrowConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a borrow configuration.
--
-- /See:/ 'newBorrowConfiguration' smart constructor.
data BorrowConfiguration = BorrowConfiguration'
  { -- | Indicates whether early check-ins are allowed.
    allowEarlyCheckIn :: Prelude.Bool,
    -- | Maximum time for the borrow configuration, in minutes.
    maxTimeToLiveInMinutes :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BorrowConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowEarlyCheckIn', 'borrowConfiguration_allowEarlyCheckIn' - Indicates whether early check-ins are allowed.
--
-- 'maxTimeToLiveInMinutes', 'borrowConfiguration_maxTimeToLiveInMinutes' - Maximum time for the borrow configuration, in minutes.
newBorrowConfiguration ::
  -- | 'allowEarlyCheckIn'
  Prelude.Bool ->
  -- | 'maxTimeToLiveInMinutes'
  Prelude.Int ->
  BorrowConfiguration
newBorrowConfiguration
  pAllowEarlyCheckIn_
  pMaxTimeToLiveInMinutes_ =
    BorrowConfiguration'
      { allowEarlyCheckIn =
          pAllowEarlyCheckIn_,
        maxTimeToLiveInMinutes = pMaxTimeToLiveInMinutes_
      }

-- | Indicates whether early check-ins are allowed.
borrowConfiguration_allowEarlyCheckIn :: Lens.Lens' BorrowConfiguration Prelude.Bool
borrowConfiguration_allowEarlyCheckIn = Lens.lens (\BorrowConfiguration' {allowEarlyCheckIn} -> allowEarlyCheckIn) (\s@BorrowConfiguration' {} a -> s {allowEarlyCheckIn = a} :: BorrowConfiguration)

-- | Maximum time for the borrow configuration, in minutes.
borrowConfiguration_maxTimeToLiveInMinutes :: Lens.Lens' BorrowConfiguration Prelude.Int
borrowConfiguration_maxTimeToLiveInMinutes = Lens.lens (\BorrowConfiguration' {maxTimeToLiveInMinutes} -> maxTimeToLiveInMinutes) (\s@BorrowConfiguration' {} a -> s {maxTimeToLiveInMinutes = a} :: BorrowConfiguration)

instance Data.FromJSON BorrowConfiguration where
  parseJSON =
    Data.withObject
      "BorrowConfiguration"
      ( \x ->
          BorrowConfiguration'
            Prelude.<$> (x Data..: "AllowEarlyCheckIn")
            Prelude.<*> (x Data..: "MaxTimeToLiveInMinutes")
      )

instance Prelude.Hashable BorrowConfiguration where
  hashWithSalt _salt BorrowConfiguration' {..} =
    _salt `Prelude.hashWithSalt` allowEarlyCheckIn
      `Prelude.hashWithSalt` maxTimeToLiveInMinutes

instance Prelude.NFData BorrowConfiguration where
  rnf BorrowConfiguration' {..} =
    Prelude.rnf allowEarlyCheckIn
      `Prelude.seq` Prelude.rnf maxTimeToLiveInMinutes

instance Data.ToJSON BorrowConfiguration where
  toJSON BorrowConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AllowEarlyCheckIn" Data..= allowEarlyCheckIn),
            Prelude.Just
              ( "MaxTimeToLiveInMinutes"
                  Data..= maxTimeToLiveInMinutes
              )
          ]
      )
