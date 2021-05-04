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
-- Module      : Network.AWS.SSM.Types.FailedCreateAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.FailedCreateAssociation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.Fault

-- | Describes a failed association.
--
-- /See:/ 'newFailedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { -- | The association.
    entry :: Prelude.Maybe CreateAssociationBatchRequestEntry,
    -- | A description of the failure.
    message :: Prelude.Maybe Prelude.Text,
    -- | The source of the failure.
    fault :: Prelude.Maybe Fault
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedCreateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entry', 'failedCreateAssociation_entry' - The association.
--
-- 'message', 'failedCreateAssociation_message' - A description of the failure.
--
-- 'fault', 'failedCreateAssociation_fault' - The source of the failure.
newFailedCreateAssociation ::
  FailedCreateAssociation
newFailedCreateAssociation =
  FailedCreateAssociation'
    { entry = Prelude.Nothing,
      message = Prelude.Nothing,
      fault = Prelude.Nothing
    }

-- | The association.
failedCreateAssociation_entry :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe CreateAssociationBatchRequestEntry)
failedCreateAssociation_entry = Lens.lens (\FailedCreateAssociation' {entry} -> entry) (\s@FailedCreateAssociation' {} a -> s {entry = a} :: FailedCreateAssociation)

-- | A description of the failure.
failedCreateAssociation_message :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe Prelude.Text)
failedCreateAssociation_message = Lens.lens (\FailedCreateAssociation' {message} -> message) (\s@FailedCreateAssociation' {} a -> s {message = a} :: FailedCreateAssociation)

-- | The source of the failure.
failedCreateAssociation_fault :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe Fault)
failedCreateAssociation_fault = Lens.lens (\FailedCreateAssociation' {fault} -> fault) (\s@FailedCreateAssociation' {} a -> s {fault = a} :: FailedCreateAssociation)

instance Prelude.FromJSON FailedCreateAssociation where
  parseJSON =
    Prelude.withObject
      "FailedCreateAssociation"
      ( \x ->
          FailedCreateAssociation'
            Prelude.<$> (x Prelude..:? "Entry")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Fault")
      )

instance Prelude.Hashable FailedCreateAssociation

instance Prelude.NFData FailedCreateAssociation
