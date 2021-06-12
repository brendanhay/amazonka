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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.TimeToLiveStatus
import qualified Network.AWS.Lens as Lens

-- | The description of the Time to Live (TTL) status on the specified table.
--
-- /See:/ 'newTimeToLiveDescription' smart constructor.
data TimeToLiveDescription = TimeToLiveDescription'
  { -- | The TTL status for the table.
    timeToLiveStatus :: Core.Maybe TimeToLiveStatus,
    -- | The name of the TTL attribute for items in the table.
    attributeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      attributeName = Core.Nothing
    }

-- | The TTL status for the table.
timeToLiveDescription_timeToLiveStatus :: Lens.Lens' TimeToLiveDescription (Core.Maybe TimeToLiveStatus)
timeToLiveDescription_timeToLiveStatus = Lens.lens (\TimeToLiveDescription' {timeToLiveStatus} -> timeToLiveStatus) (\s@TimeToLiveDescription' {} a -> s {timeToLiveStatus = a} :: TimeToLiveDescription)

-- | The name of the TTL attribute for items in the table.
timeToLiveDescription_attributeName :: Lens.Lens' TimeToLiveDescription (Core.Maybe Core.Text)
timeToLiveDescription_attributeName = Lens.lens (\TimeToLiveDescription' {attributeName} -> attributeName) (\s@TimeToLiveDescription' {} a -> s {attributeName = a} :: TimeToLiveDescription)

instance Core.FromJSON TimeToLiveDescription where
  parseJSON =
    Core.withObject
      "TimeToLiveDescription"
      ( \x ->
          TimeToLiveDescription'
            Core.<$> (x Core..:? "TimeToLiveStatus")
            Core.<*> (x Core..:? "AttributeName")
      )

instance Core.Hashable TimeToLiveDescription

instance Core.NFData TimeToLiveDescription
