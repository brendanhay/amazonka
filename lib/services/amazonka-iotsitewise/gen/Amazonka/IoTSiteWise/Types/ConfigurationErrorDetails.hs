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
-- Module      : Amazonka.IoTSiteWise.Types.ConfigurationErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ConfigurationErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an IoT SiteWise configuration error.
--
-- /See:/ 'newConfigurationErrorDetails' smart constructor.
data ConfigurationErrorDetails = ConfigurationErrorDetails'
  { -- | The error code.
    code :: ErrorCode,
    -- | The error message.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'configurationErrorDetails_code' - The error code.
--
-- 'message', 'configurationErrorDetails_message' - The error message.
newConfigurationErrorDetails ::
  -- | 'code'
  ErrorCode ->
  -- | 'message'
  Prelude.Text ->
  ConfigurationErrorDetails
newConfigurationErrorDetails pCode_ pMessage_ =
  ConfigurationErrorDetails'
    { code = pCode_,
      message = pMessage_
    }

-- | The error code.
configurationErrorDetails_code :: Lens.Lens' ConfigurationErrorDetails ErrorCode
configurationErrorDetails_code = Lens.lens (\ConfigurationErrorDetails' {code} -> code) (\s@ConfigurationErrorDetails' {} a -> s {code = a} :: ConfigurationErrorDetails)

-- | The error message.
configurationErrorDetails_message :: Lens.Lens' ConfigurationErrorDetails Prelude.Text
configurationErrorDetails_message = Lens.lens (\ConfigurationErrorDetails' {message} -> message) (\s@ConfigurationErrorDetails' {} a -> s {message = a} :: ConfigurationErrorDetails)

instance Data.FromJSON ConfigurationErrorDetails where
  parseJSON =
    Data.withObject
      "ConfigurationErrorDetails"
      ( \x ->
          ConfigurationErrorDetails'
            Prelude.<$> (x Data..: "code")
            Prelude.<*> (x Data..: "message")
      )

instance Prelude.Hashable ConfigurationErrorDetails where
  hashWithSalt _salt ConfigurationErrorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ConfigurationErrorDetails where
  rnf ConfigurationErrorDetails' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
