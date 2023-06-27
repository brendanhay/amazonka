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
-- Module      : Amazonka.SSMContacts.Types.ShiftDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.ShiftDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about overrides to an on-call rotation shift.
--
-- /See:/ 'newShiftDetails' smart constructor.
data ShiftDetails = ShiftDetails'
  { -- | The Amazon Resources Names (ARNs) of the contacts who were replaced in a
    -- shift when an override was created. If the override is deleted, these
    -- contacts are restored to the shift.
    overriddenContactIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShiftDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overriddenContactIds', 'shiftDetails_overriddenContactIds' - The Amazon Resources Names (ARNs) of the contacts who were replaced in a
-- shift when an override was created. If the override is deleted, these
-- contacts are restored to the shift.
newShiftDetails ::
  ShiftDetails
newShiftDetails =
  ShiftDetails'
    { overriddenContactIds =
        Prelude.mempty
    }

-- | The Amazon Resources Names (ARNs) of the contacts who were replaced in a
-- shift when an override was created. If the override is deleted, these
-- contacts are restored to the shift.
shiftDetails_overriddenContactIds :: Lens.Lens' ShiftDetails [Prelude.Text]
shiftDetails_overriddenContactIds = Lens.lens (\ShiftDetails' {overriddenContactIds} -> overriddenContactIds) (\s@ShiftDetails' {} a -> s {overriddenContactIds = a} :: ShiftDetails) Prelude.. Lens.coerced

instance Data.FromJSON ShiftDetails where
  parseJSON =
    Data.withObject
      "ShiftDetails"
      ( \x ->
          ShiftDetails'
            Prelude.<$> ( x
                            Data..:? "OverriddenContactIds"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ShiftDetails where
  hashWithSalt _salt ShiftDetails' {..} =
    _salt `Prelude.hashWithSalt` overriddenContactIds

instance Prelude.NFData ShiftDetails where
  rnf ShiftDetails' {..} =
    Prelude.rnf overriddenContactIds
