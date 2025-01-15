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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AutoTuneOptions
import Amazonka.OpenSearch.Types.AutoTuneStatus
import qualified Amazonka.Prelude as Prelude

-- | The Auto-Tune status for the domain.
--
-- /See:/ 'newAutoTuneOptionsStatus' smart constructor.
data AutoTuneOptionsStatus = AutoTuneOptionsStatus'
  { -- | Auto-Tune settings for updating a domain.
    options :: Prelude.Maybe AutoTuneOptions,
    -- | The current status of Auto-Tune for a domain.
    status :: Prelude.Maybe AutoTuneStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'autoTuneOptionsStatus_options' - Auto-Tune settings for updating a domain.
--
-- 'status', 'autoTuneOptionsStatus_status' - The current status of Auto-Tune for a domain.
newAutoTuneOptionsStatus ::
  AutoTuneOptionsStatus
newAutoTuneOptionsStatus =
  AutoTuneOptionsStatus'
    { options = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Auto-Tune settings for updating a domain.
autoTuneOptionsStatus_options :: Lens.Lens' AutoTuneOptionsStatus (Prelude.Maybe AutoTuneOptions)
autoTuneOptionsStatus_options = Lens.lens (\AutoTuneOptionsStatus' {options} -> options) (\s@AutoTuneOptionsStatus' {} a -> s {options = a} :: AutoTuneOptionsStatus)

-- | The current status of Auto-Tune for a domain.
autoTuneOptionsStatus_status :: Lens.Lens' AutoTuneOptionsStatus (Prelude.Maybe AutoTuneStatus)
autoTuneOptionsStatus_status = Lens.lens (\AutoTuneOptionsStatus' {status} -> status) (\s@AutoTuneOptionsStatus' {} a -> s {status = a} :: AutoTuneOptionsStatus)

instance Data.FromJSON AutoTuneOptionsStatus where
  parseJSON =
    Data.withObject
      "AutoTuneOptionsStatus"
      ( \x ->
          AutoTuneOptionsStatus'
            Prelude.<$> (x Data..:? "Options")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AutoTuneOptionsStatus where
  hashWithSalt _salt AutoTuneOptionsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData AutoTuneOptionsStatus where
  rnf AutoTuneOptionsStatus' {..} =
    Prelude.rnf options `Prelude.seq`
      Prelude.rnf status
