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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.FailedCreateAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.CreateAssociationBatchRequestEntry
import Amazonka.SSM.Types.Fault

-- | Describes a failed association.
--
-- /See:/ 'newFailedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { -- | The association.
    entry :: Prelude.Maybe CreateAssociationBatchRequestEntry,
    -- | The source of the failure.
    fault :: Prelude.Maybe Fault,
    -- | A description of the failure.
    message :: Prelude.Maybe Prelude.Text
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
-- 'entry', 'failedCreateAssociation_entry' - The association.
--
-- 'fault', 'failedCreateAssociation_fault' - The source of the failure.
--
-- 'message', 'failedCreateAssociation_message' - A description of the failure.
newFailedCreateAssociation ::
  FailedCreateAssociation
newFailedCreateAssociation =
  FailedCreateAssociation'
    { entry = Prelude.Nothing,
      fault = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The association.
failedCreateAssociation_entry :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe CreateAssociationBatchRequestEntry)
failedCreateAssociation_entry = Lens.lens (\FailedCreateAssociation' {entry} -> entry) (\s@FailedCreateAssociation' {} a -> s {entry = a} :: FailedCreateAssociation)

-- | The source of the failure.
failedCreateAssociation_fault :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe Fault)
failedCreateAssociation_fault = Lens.lens (\FailedCreateAssociation' {fault} -> fault) (\s@FailedCreateAssociation' {} a -> s {fault = a} :: FailedCreateAssociation)

-- | A description of the failure.
failedCreateAssociation_message :: Lens.Lens' FailedCreateAssociation (Prelude.Maybe Prelude.Text)
failedCreateAssociation_message = Lens.lens (\FailedCreateAssociation' {message} -> message) (\s@FailedCreateAssociation' {} a -> s {message = a} :: FailedCreateAssociation)

instance Data.FromJSON FailedCreateAssociation where
  parseJSON =
    Data.withObject
      "FailedCreateAssociation"
      ( \x ->
          FailedCreateAssociation'
            Prelude.<$> (x Data..:? "Entry")
            Prelude.<*> (x Data..:? "Fault")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable FailedCreateAssociation where
  hashWithSalt _salt FailedCreateAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` entry
      `Prelude.hashWithSalt` fault
      `Prelude.hashWithSalt` message

instance Prelude.NFData FailedCreateAssociation where
  rnf FailedCreateAssociation' {..} =
    Prelude.rnf entry
      `Prelude.seq` Prelude.rnf fault
      `Prelude.seq` Prelude.rnf message
