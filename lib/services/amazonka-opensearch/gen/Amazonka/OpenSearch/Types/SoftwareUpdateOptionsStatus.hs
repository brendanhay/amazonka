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
-- Module      : Amazonka.OpenSearch.Types.SoftwareUpdateOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.SoftwareUpdateOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OptionStatus
import Amazonka.OpenSearch.Types.SoftwareUpdateOptions
import qualified Amazonka.Prelude as Prelude

-- | The status of the service software options for a domain.
--
-- /See:/ 'newSoftwareUpdateOptionsStatus' smart constructor.
data SoftwareUpdateOptionsStatus = SoftwareUpdateOptionsStatus'
  { -- | The service software update options for a domain.
    options :: Prelude.Maybe SoftwareUpdateOptions,
    -- | The status of service software update options, including creation date
    -- and last updated date.
    status :: Prelude.Maybe OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SoftwareUpdateOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'softwareUpdateOptionsStatus_options' - The service software update options for a domain.
--
-- 'status', 'softwareUpdateOptionsStatus_status' - The status of service software update options, including creation date
-- and last updated date.
newSoftwareUpdateOptionsStatus ::
  SoftwareUpdateOptionsStatus
newSoftwareUpdateOptionsStatus =
  SoftwareUpdateOptionsStatus'
    { options =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The service software update options for a domain.
softwareUpdateOptionsStatus_options :: Lens.Lens' SoftwareUpdateOptionsStatus (Prelude.Maybe SoftwareUpdateOptions)
softwareUpdateOptionsStatus_options = Lens.lens (\SoftwareUpdateOptionsStatus' {options} -> options) (\s@SoftwareUpdateOptionsStatus' {} a -> s {options = a} :: SoftwareUpdateOptionsStatus)

-- | The status of service software update options, including creation date
-- and last updated date.
softwareUpdateOptionsStatus_status :: Lens.Lens' SoftwareUpdateOptionsStatus (Prelude.Maybe OptionStatus)
softwareUpdateOptionsStatus_status = Lens.lens (\SoftwareUpdateOptionsStatus' {status} -> status) (\s@SoftwareUpdateOptionsStatus' {} a -> s {status = a} :: SoftwareUpdateOptionsStatus)

instance Data.FromJSON SoftwareUpdateOptionsStatus where
  parseJSON =
    Data.withObject
      "SoftwareUpdateOptionsStatus"
      ( \x ->
          SoftwareUpdateOptionsStatus'
            Prelude.<$> (x Data..:? "Options")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable SoftwareUpdateOptionsStatus where
  hashWithSalt _salt SoftwareUpdateOptionsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData SoftwareUpdateOptionsStatus where
  rnf SoftwareUpdateOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
