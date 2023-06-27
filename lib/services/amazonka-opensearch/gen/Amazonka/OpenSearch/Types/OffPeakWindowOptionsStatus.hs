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
-- Module      : Amazonka.OpenSearch.Types.OffPeakWindowOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OffPeakWindowOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OffPeakWindowOptions
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of
-- <https://docs.aws.amazon.com/opensearch-service/latest/APIReference/API_OffPeakWindow.html off-peak window>
-- options for a domain.
--
-- /See:/ 'newOffPeakWindowOptionsStatus' smart constructor.
data OffPeakWindowOptionsStatus = OffPeakWindowOptionsStatus'
  { -- | The domain\'s off-peak window configuration.
    options :: Prelude.Maybe OffPeakWindowOptions,
    -- | The current status of off-peak window options.
    status :: Prelude.Maybe OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OffPeakWindowOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'offPeakWindowOptionsStatus_options' - The domain\'s off-peak window configuration.
--
-- 'status', 'offPeakWindowOptionsStatus_status' - The current status of off-peak window options.
newOffPeakWindowOptionsStatus ::
  OffPeakWindowOptionsStatus
newOffPeakWindowOptionsStatus =
  OffPeakWindowOptionsStatus'
    { options =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The domain\'s off-peak window configuration.
offPeakWindowOptionsStatus_options :: Lens.Lens' OffPeakWindowOptionsStatus (Prelude.Maybe OffPeakWindowOptions)
offPeakWindowOptionsStatus_options = Lens.lens (\OffPeakWindowOptionsStatus' {options} -> options) (\s@OffPeakWindowOptionsStatus' {} a -> s {options = a} :: OffPeakWindowOptionsStatus)

-- | The current status of off-peak window options.
offPeakWindowOptionsStatus_status :: Lens.Lens' OffPeakWindowOptionsStatus (Prelude.Maybe OptionStatus)
offPeakWindowOptionsStatus_status = Lens.lens (\OffPeakWindowOptionsStatus' {status} -> status) (\s@OffPeakWindowOptionsStatus' {} a -> s {status = a} :: OffPeakWindowOptionsStatus)

instance Data.FromJSON OffPeakWindowOptionsStatus where
  parseJSON =
    Data.withObject
      "OffPeakWindowOptionsStatus"
      ( \x ->
          OffPeakWindowOptionsStatus'
            Prelude.<$> (x Data..:? "Options")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable OffPeakWindowOptionsStatus where
  hashWithSalt _salt OffPeakWindowOptionsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData OffPeakWindowOptionsStatus where
  rnf OffPeakWindowOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
