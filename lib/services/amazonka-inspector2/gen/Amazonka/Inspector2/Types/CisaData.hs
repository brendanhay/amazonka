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
-- Module      : Amazonka.Inspector2.Types.CisaData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CisaData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Cybersecurity and Infrastructure Security Agency (CISA) details for
-- a specific vulnerability.
--
-- /See:/ 'newCisaData' smart constructor.
data CisaData = CisaData'
  { -- | The remediation action recommended by CISA for this vulnerability.
    action :: Prelude.Maybe Prelude.Text,
    -- | The date and time CISA added this vulnerability to their catalogue.
    dateAdded :: Prelude.Maybe Data.POSIX,
    -- | The date and time CISA expects a fix to have been provided
    -- vulnerability.
    dateDue :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CisaData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'cisaData_action' - The remediation action recommended by CISA for this vulnerability.
--
-- 'dateAdded', 'cisaData_dateAdded' - The date and time CISA added this vulnerability to their catalogue.
--
-- 'dateDue', 'cisaData_dateDue' - The date and time CISA expects a fix to have been provided
-- vulnerability.
newCisaData ::
  CisaData
newCisaData =
  CisaData'
    { action = Prelude.Nothing,
      dateAdded = Prelude.Nothing,
      dateDue = Prelude.Nothing
    }

-- | The remediation action recommended by CISA for this vulnerability.
cisaData_action :: Lens.Lens' CisaData (Prelude.Maybe Prelude.Text)
cisaData_action = Lens.lens (\CisaData' {action} -> action) (\s@CisaData' {} a -> s {action = a} :: CisaData)

-- | The date and time CISA added this vulnerability to their catalogue.
cisaData_dateAdded :: Lens.Lens' CisaData (Prelude.Maybe Prelude.UTCTime)
cisaData_dateAdded = Lens.lens (\CisaData' {dateAdded} -> dateAdded) (\s@CisaData' {} a -> s {dateAdded = a} :: CisaData) Prelude.. Lens.mapping Data._Time

-- | The date and time CISA expects a fix to have been provided
-- vulnerability.
cisaData_dateDue :: Lens.Lens' CisaData (Prelude.Maybe Prelude.UTCTime)
cisaData_dateDue = Lens.lens (\CisaData' {dateDue} -> dateDue) (\s@CisaData' {} a -> s {dateDue = a} :: CisaData) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CisaData where
  parseJSON =
    Data.withObject
      "CisaData"
      ( \x ->
          CisaData'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "dateAdded")
            Prelude.<*> (x Data..:? "dateDue")
      )

instance Prelude.Hashable CisaData where
  hashWithSalt _salt CisaData' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` dateAdded
      `Prelude.hashWithSalt` dateDue

instance Prelude.NFData CisaData where
  rnf CisaData' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf dateAdded
      `Prelude.seq` Prelude.rnf dateDue
