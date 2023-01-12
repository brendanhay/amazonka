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
-- Module      : Amazonka.IoTSiteWise.Types.MonitorErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.MonitorErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.MonitorErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains IoT SiteWise Monitor error details.
--
-- /See:/ 'newMonitorErrorDetails' smart constructor.
data MonitorErrorDetails = MonitorErrorDetails'
  { -- | The error code.
    code :: Prelude.Maybe MonitorErrorCode,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'monitorErrorDetails_code' - The error code.
--
-- 'message', 'monitorErrorDetails_message' - The error message.
newMonitorErrorDetails ::
  MonitorErrorDetails
newMonitorErrorDetails =
  MonitorErrorDetails'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
monitorErrorDetails_code :: Lens.Lens' MonitorErrorDetails (Prelude.Maybe MonitorErrorCode)
monitorErrorDetails_code = Lens.lens (\MonitorErrorDetails' {code} -> code) (\s@MonitorErrorDetails' {} a -> s {code = a} :: MonitorErrorDetails)

-- | The error message.
monitorErrorDetails_message :: Lens.Lens' MonitorErrorDetails (Prelude.Maybe Prelude.Text)
monitorErrorDetails_message = Lens.lens (\MonitorErrorDetails' {message} -> message) (\s@MonitorErrorDetails' {} a -> s {message = a} :: MonitorErrorDetails)

instance Data.FromJSON MonitorErrorDetails where
  parseJSON =
    Data.withObject
      "MonitorErrorDetails"
      ( \x ->
          MonitorErrorDetails'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable MonitorErrorDetails where
  hashWithSalt _salt MonitorErrorDetails' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData MonitorErrorDetails where
  rnf MonitorErrorDetails' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
