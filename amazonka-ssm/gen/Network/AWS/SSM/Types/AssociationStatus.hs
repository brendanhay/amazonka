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
-- Module      : Network.AWS.SSM.Types.AssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AssociationStatusName

-- | Describes an association status.
--
-- /See:/ 'newAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { -- | A user-defined string.
    additionalInfo :: Core.Maybe Core.Text,
    -- | The date when the status changed.
    date :: Core.POSIX,
    -- | The status.
    name :: AssociationStatusName,
    -- | The reason for the status.
    message :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'associationStatus_additionalInfo' - A user-defined string.
--
-- 'date', 'associationStatus_date' - The date when the status changed.
--
-- 'name', 'associationStatus_name' - The status.
--
-- 'message', 'associationStatus_message' - The reason for the status.
newAssociationStatus ::
  -- | 'date'
  Core.UTCTime ->
  -- | 'name'
  AssociationStatusName ->
  -- | 'message'
  Core.Text ->
  AssociationStatus
newAssociationStatus pDate_ pName_ pMessage_ =
  AssociationStatus'
    { additionalInfo = Core.Nothing,
      date = Core._Time Lens.# pDate_,
      name = pName_,
      message = pMessage_
    }

-- | A user-defined string.
associationStatus_additionalInfo :: Lens.Lens' AssociationStatus (Core.Maybe Core.Text)
associationStatus_additionalInfo = Lens.lens (\AssociationStatus' {additionalInfo} -> additionalInfo) (\s@AssociationStatus' {} a -> s {additionalInfo = a} :: AssociationStatus)

-- | The date when the status changed.
associationStatus_date :: Lens.Lens' AssociationStatus Core.UTCTime
associationStatus_date = Lens.lens (\AssociationStatus' {date} -> date) (\s@AssociationStatus' {} a -> s {date = a} :: AssociationStatus) Core.. Core._Time

-- | The status.
associationStatus_name :: Lens.Lens' AssociationStatus AssociationStatusName
associationStatus_name = Lens.lens (\AssociationStatus' {name} -> name) (\s@AssociationStatus' {} a -> s {name = a} :: AssociationStatus)

-- | The reason for the status.
associationStatus_message :: Lens.Lens' AssociationStatus Core.Text
associationStatus_message = Lens.lens (\AssociationStatus' {message} -> message) (\s@AssociationStatus' {} a -> s {message = a} :: AssociationStatus)

instance Core.FromJSON AssociationStatus where
  parseJSON =
    Core.withObject
      "AssociationStatus"
      ( \x ->
          AssociationStatus'
            Core.<$> (x Core..:? "AdditionalInfo")
            Core.<*> (x Core..: "Date")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Message")
      )

instance Core.Hashable AssociationStatus

instance Core.NFData AssociationStatus

instance Core.ToJSON AssociationStatus where
  toJSON AssociationStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AdditionalInfo" Core..=) Core.<$> additionalInfo,
            Core.Just ("Date" Core..= date),
            Core.Just ("Name" Core..= name),
            Core.Just ("Message" Core..= message)
          ]
      )
