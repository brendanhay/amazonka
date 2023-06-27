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
-- Module      : Amazonka.SecurityHub.Types.FindingHistoryUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingHistoryUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An array of objects that provides details about a change to a finding,
-- including the Amazon Web Services Security Finding Format (ASFF) field
-- that changed, the value of the field before the change, and the value of
-- the field after the change.
--
-- /See:/ 'newFindingHistoryUpdate' smart constructor.
data FindingHistoryUpdate = FindingHistoryUpdate'
  { -- | The value of the ASFF field after the finding change event. To preserve
    -- storage and readability, Security Hub omits this value if
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_FindingHistoryRecord.html FindingHistoryRecord>
    -- exceeds database limits.
    newValue' :: Prelude.Maybe Prelude.Text,
    -- | The value of the ASFF field before the finding change event.
    oldValue :: Prelude.Maybe Prelude.Text,
    -- | The ASFF field that changed during the finding change event.
    updatedField :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingHistoryUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newValue'', 'findingHistoryUpdate_newValue' - The value of the ASFF field after the finding change event. To preserve
-- storage and readability, Security Hub omits this value if
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_FindingHistoryRecord.html FindingHistoryRecord>
-- exceeds database limits.
--
-- 'oldValue', 'findingHistoryUpdate_oldValue' - The value of the ASFF field before the finding change event.
--
-- 'updatedField', 'findingHistoryUpdate_updatedField' - The ASFF field that changed during the finding change event.
newFindingHistoryUpdate ::
  FindingHistoryUpdate
newFindingHistoryUpdate =
  FindingHistoryUpdate'
    { newValue' = Prelude.Nothing,
      oldValue = Prelude.Nothing,
      updatedField = Prelude.Nothing
    }

-- | The value of the ASFF field after the finding change event. To preserve
-- storage and readability, Security Hub omits this value if
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_FindingHistoryRecord.html FindingHistoryRecord>
-- exceeds database limits.
findingHistoryUpdate_newValue :: Lens.Lens' FindingHistoryUpdate (Prelude.Maybe Prelude.Text)
findingHistoryUpdate_newValue = Lens.lens (\FindingHistoryUpdate' {newValue'} -> newValue') (\s@FindingHistoryUpdate' {} a -> s {newValue' = a} :: FindingHistoryUpdate)

-- | The value of the ASFF field before the finding change event.
findingHistoryUpdate_oldValue :: Lens.Lens' FindingHistoryUpdate (Prelude.Maybe Prelude.Text)
findingHistoryUpdate_oldValue = Lens.lens (\FindingHistoryUpdate' {oldValue} -> oldValue) (\s@FindingHistoryUpdate' {} a -> s {oldValue = a} :: FindingHistoryUpdate)

-- | The ASFF field that changed during the finding change event.
findingHistoryUpdate_updatedField :: Lens.Lens' FindingHistoryUpdate (Prelude.Maybe Prelude.Text)
findingHistoryUpdate_updatedField = Lens.lens (\FindingHistoryUpdate' {updatedField} -> updatedField) (\s@FindingHistoryUpdate' {} a -> s {updatedField = a} :: FindingHistoryUpdate)

instance Data.FromJSON FindingHistoryUpdate where
  parseJSON =
    Data.withObject
      "FindingHistoryUpdate"
      ( \x ->
          FindingHistoryUpdate'
            Prelude.<$> (x Data..:? "NewValue")
            Prelude.<*> (x Data..:? "OldValue")
            Prelude.<*> (x Data..:? "UpdatedField")
      )

instance Prelude.Hashable FindingHistoryUpdate where
  hashWithSalt _salt FindingHistoryUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` newValue'
      `Prelude.hashWithSalt` oldValue
      `Prelude.hashWithSalt` updatedField

instance Prelude.NFData FindingHistoryUpdate where
  rnf FindingHistoryUpdate' {..} =
    Prelude.rnf newValue'
      `Prelude.seq` Prelude.rnf oldValue
      `Prelude.seq` Prelude.rnf updatedField
