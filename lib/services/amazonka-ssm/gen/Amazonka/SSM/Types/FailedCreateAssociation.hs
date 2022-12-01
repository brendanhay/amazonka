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
-- Module      : Amazonka.SSM.Types.FailedCreateAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.FailedCreateAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.CreateAssociationBatchRequestEntry
import Amazonka.SSM.Types.Fault

-- | Describes a failed association.
--
-- /See:/ 'newFailedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { -- | A description of the failure.
    message :: Prelude.Maybe Prelude.Text,
    -- | The source of the failure.
    fault :: Prelude.Maybe Fault,
    -- | The association.
    entry :: Prelude.Maybe CreateAssociationBatchRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCreateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'failedCreateAssociation_message' - A description of the failure.
--
-- 'fault', 'failedCreateAssociation_fault' - The source of the failure.
--
-- 'entry', 'failedCreateAssociation_entry' - The association.
newFailedCreateAssociation ::
  FailedCreateAssociation
newFailedCreateAssociation =
  FailedCreateAssociation'
    { message = Prelude.Nothing,
      fault = Prelude.Nothing,
      entry = Prelude.Nothing
    }

-- | A description of the failure.
failedCreateAssociation_message :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe Prelude.Text)
failedCreateAssociation_message = Lens.lens (\FailedCreateAssociation' {message} -> message) (\s@FailedCreateAssociation' {} a -> s {message = a} :: FailedCreateAssociation)

-- | The source of the failure.
failedCreateAssociation_fault :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe Fault)
failedCreateAssociation_fault = Lens.lens (\FailedCreateAssociation' {fault} -> fault) (\s@FailedCreateAssociation' {} a -> s {fault = a} :: FailedCreateAssociation)

-- | The association.
failedCreateAssociation_entry :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe CreateAssociationBatchRequestEntry)
failedCreateAssociation_entry = Lens.lens (\FailedCreateAssociation' {entry} -> entry) (\s@FailedCreateAssociation' {} a -> s {entry = a} :: FailedCreateAssociation)

instance Core.FromJSON FailedCreateAssociation where
  parseJSON =
    Core.withObject
      "FailedCreateAssociation"
      ( \x ->
          FailedCreateAssociation'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Fault")
            Prelude.<*> (x Core..:? "Entry")
      )

instance Prelude.Hashable FailedCreateAssociation where
  hashWithSalt _salt FailedCreateAssociation' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` fault
      `Prelude.hashWithSalt` entry

instance Prelude.NFData FailedCreateAssociation where
  rnf FailedCreateAssociation' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf fault
      `Prelude.seq` Prelude.rnf entry
