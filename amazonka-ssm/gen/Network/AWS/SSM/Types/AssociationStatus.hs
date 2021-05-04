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
-- Module      : Network.AWS.SSM.Types.AssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AssociationStatusName

-- | Describes an association status.
--
-- /See:/ 'newAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { -- | A user-defined string.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | The date when the status changed.
    date :: Prelude.POSIX,
    -- | The status.
    name :: AssociationStatusName,
    -- | The reason for the status.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.UTCTime ->
  -- | 'name'
  AssociationStatusName ->
  -- | 'message'
  Prelude.Text ->
  AssociationStatus
newAssociationStatus pDate_ pName_ pMessage_ =
  AssociationStatus'
    { additionalInfo =
        Prelude.Nothing,
      date = Prelude._Time Lens.# pDate_,
      name = pName_,
      message = pMessage_
    }

-- | A user-defined string.
associationStatus_additionalInfo :: Lens.Lens' AssociationStatus (Prelude.Maybe Prelude.Text)
associationStatus_additionalInfo = Lens.lens (\AssociationStatus' {additionalInfo} -> additionalInfo) (\s@AssociationStatus' {} a -> s {additionalInfo = a} :: AssociationStatus)

-- | The date when the status changed.
associationStatus_date :: Lens.Lens' AssociationStatus Prelude.UTCTime
associationStatus_date = Lens.lens (\AssociationStatus' {date} -> date) (\s@AssociationStatus' {} a -> s {date = a} :: AssociationStatus) Prelude.. Prelude._Time

-- | The status.
associationStatus_name :: Lens.Lens' AssociationStatus AssociationStatusName
associationStatus_name = Lens.lens (\AssociationStatus' {name} -> name) (\s@AssociationStatus' {} a -> s {name = a} :: AssociationStatus)

-- | The reason for the status.
associationStatus_message :: Lens.Lens' AssociationStatus Prelude.Text
associationStatus_message = Lens.lens (\AssociationStatus' {message} -> message) (\s@AssociationStatus' {} a -> s {message = a} :: AssociationStatus)

instance Prelude.FromJSON AssociationStatus where
  parseJSON =
    Prelude.withObject
      "AssociationStatus"
      ( \x ->
          AssociationStatus'
            Prelude.<$> (x Prelude..:? "AdditionalInfo")
            Prelude.<*> (x Prelude..: "Date")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Message")
      )

instance Prelude.Hashable AssociationStatus

instance Prelude.NFData AssociationStatus

instance Prelude.ToJSON AssociationStatus where
  toJSON AssociationStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AdditionalInfo" Prelude..=)
              Prelude.<$> additionalInfo,
            Prelude.Just ("Date" Prelude..= date),
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Message" Prelude..= message)
          ]
      )
