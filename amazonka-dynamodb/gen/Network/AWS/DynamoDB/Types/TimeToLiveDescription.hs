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
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveDescription where

import Network.AWS.DynamoDB.Types.TimeToLiveStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The description of the Time to Live (TTL) status on the specified table.
--
-- /See:/ 'newTimeToLiveDescription' smart constructor.
data TimeToLiveDescription = TimeToLiveDescription'
  { -- | The TTL status for the table.
    timeToLiveStatus :: Prelude.Maybe TimeToLiveStatus,
    -- | The name of the TTL attribute for items in the table.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimeToLiveDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLiveStatus', 'timeToLiveDescription_timeToLiveStatus' - The TTL status for the table.
--
-- 'attributeName', 'timeToLiveDescription_attributeName' - The name of the TTL attribute for items in the table.
newTimeToLiveDescription ::
  TimeToLiveDescription
newTimeToLiveDescription =
  TimeToLiveDescription'
    { timeToLiveStatus =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The TTL status for the table.
timeToLiveDescription_timeToLiveStatus :: Lens.Lens' TimeToLiveDescription (Prelude.Maybe TimeToLiveStatus)
timeToLiveDescription_timeToLiveStatus = Lens.lens (\TimeToLiveDescription' {timeToLiveStatus} -> timeToLiveStatus) (\s@TimeToLiveDescription' {} a -> s {timeToLiveStatus = a} :: TimeToLiveDescription)

-- | The name of the TTL attribute for items in the table.
timeToLiveDescription_attributeName :: Lens.Lens' TimeToLiveDescription (Prelude.Maybe Prelude.Text)
timeToLiveDescription_attributeName = Lens.lens (\TimeToLiveDescription' {attributeName} -> attributeName) (\s@TimeToLiveDescription' {} a -> s {attributeName = a} :: TimeToLiveDescription)

instance Prelude.FromJSON TimeToLiveDescription where
  parseJSON =
    Prelude.withObject
      "TimeToLiveDescription"
      ( \x ->
          TimeToLiveDescription'
            Prelude.<$> (x Prelude..:? "TimeToLiveStatus")
            Prelude.<*> (x Prelude..:? "AttributeName")
      )

instance Prelude.Hashable TimeToLiveDescription

instance Prelude.NFData TimeToLiveDescription
