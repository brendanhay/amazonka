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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.Fault

-- | Describes a failed association.
--
-- /See:/ 'newFailedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { -- | The association.
    entry :: Core.Maybe CreateAssociationBatchRequestEntry,
    -- | A description of the failure.
    message :: Core.Maybe Core.Text,
    -- | The source of the failure.
    fault :: Core.Maybe Fault
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { entry = Core.Nothing,
      message = Core.Nothing,
      fault = Core.Nothing
    }

-- | The association.
failedCreateAssociation_entry :: Lens.Lens' FailedCreateAssociation (Core.Maybe CreateAssociationBatchRequestEntry)
failedCreateAssociation_entry = Lens.lens (\FailedCreateAssociation' {entry} -> entry) (\s@FailedCreateAssociation' {} a -> s {entry = a} :: FailedCreateAssociation)

-- | A description of the failure.
failedCreateAssociation_message :: Lens.Lens' FailedCreateAssociation (Core.Maybe Core.Text)
failedCreateAssociation_message = Lens.lens (\FailedCreateAssociation' {message} -> message) (\s@FailedCreateAssociation' {} a -> s {message = a} :: FailedCreateAssociation)

-- | The source of the failure.
failedCreateAssociation_fault :: Lens.Lens' FailedCreateAssociation (Core.Maybe Fault)
failedCreateAssociation_fault = Lens.lens (\FailedCreateAssociation' {fault} -> fault) (\s@FailedCreateAssociation' {} a -> s {fault = a} :: FailedCreateAssociation)

instance Core.FromJSON FailedCreateAssociation where
  parseJSON =
    Core.withObject
      "FailedCreateAssociation"
      ( \x ->
          FailedCreateAssociation'
            Core.<$> (x Core..:? "Entry")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "Fault")
      )

instance Core.Hashable FailedCreateAssociation

instance Core.NFData FailedCreateAssociation
