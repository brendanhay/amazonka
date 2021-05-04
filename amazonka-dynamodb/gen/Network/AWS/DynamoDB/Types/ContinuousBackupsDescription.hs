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
-- Module      : Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContinuousBackupsDescription where

import Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the continuous backups and point in time recovery settings on
-- the table.
--
-- /See:/ 'newContinuousBackupsDescription' smart constructor.
data ContinuousBackupsDescription = ContinuousBackupsDescription'
  { -- | The description of the point in time recovery settings applied to the
    -- table.
    pointInTimeRecoveryDescription :: Prelude.Maybe PointInTimeRecoveryDescription,
    -- | @ContinuousBackupsStatus@ can be one of the following states: ENABLED,
    -- DISABLED
    continuousBackupsStatus :: ContinuousBackupsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContinuousBackupsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pointInTimeRecoveryDescription', 'continuousBackupsDescription_pointInTimeRecoveryDescription' - The description of the point in time recovery settings applied to the
-- table.
--
-- 'continuousBackupsStatus', 'continuousBackupsDescription_continuousBackupsStatus' - @ContinuousBackupsStatus@ can be one of the following states: ENABLED,
-- DISABLED
newContinuousBackupsDescription ::
  -- | 'continuousBackupsStatus'
  ContinuousBackupsStatus ->
  ContinuousBackupsDescription
newContinuousBackupsDescription
  pContinuousBackupsStatus_ =
    ContinuousBackupsDescription'
      { pointInTimeRecoveryDescription =
          Prelude.Nothing,
        continuousBackupsStatus =
          pContinuousBackupsStatus_
      }

-- | The description of the point in time recovery settings applied to the
-- table.
continuousBackupsDescription_pointInTimeRecoveryDescription :: Lens.Lens' ContinuousBackupsDescription (Prelude.Maybe PointInTimeRecoveryDescription)
continuousBackupsDescription_pointInTimeRecoveryDescription = Lens.lens (\ContinuousBackupsDescription' {pointInTimeRecoveryDescription} -> pointInTimeRecoveryDescription) (\s@ContinuousBackupsDescription' {} a -> s {pointInTimeRecoveryDescription = a} :: ContinuousBackupsDescription)

-- | @ContinuousBackupsStatus@ can be one of the following states: ENABLED,
-- DISABLED
continuousBackupsDescription_continuousBackupsStatus :: Lens.Lens' ContinuousBackupsDescription ContinuousBackupsStatus
continuousBackupsDescription_continuousBackupsStatus = Lens.lens (\ContinuousBackupsDescription' {continuousBackupsStatus} -> continuousBackupsStatus) (\s@ContinuousBackupsDescription' {} a -> s {continuousBackupsStatus = a} :: ContinuousBackupsDescription)

instance
  Prelude.FromJSON
    ContinuousBackupsDescription
  where
  parseJSON =
    Prelude.withObject
      "ContinuousBackupsDescription"
      ( \x ->
          ContinuousBackupsDescription'
            Prelude.<$> (x Prelude..:? "PointInTimeRecoveryDescription")
            Prelude.<*> (x Prelude..: "ContinuousBackupsStatus")
      )

instance
  Prelude.Hashable
    ContinuousBackupsDescription

instance Prelude.NFData ContinuousBackupsDescription
