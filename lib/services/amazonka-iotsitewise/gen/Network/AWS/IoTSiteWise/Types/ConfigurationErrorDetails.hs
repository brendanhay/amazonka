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
-- Module      : Network.AWS.IoTSiteWise.Types.ConfigurationErrorDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.ConfigurationErrorDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.ErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON ConfigurationErrorDetails where
  parseJSON =
    Core.withObject
      "ConfigurationErrorDetails"
      ( \x ->
          ConfigurationErrorDetails'
            Prelude.<$> (x Core..: "code") Prelude.<*> (x Core..: "message")
      )

instance Prelude.Hashable ConfigurationErrorDetails

instance Prelude.NFData ConfigurationErrorDetails
