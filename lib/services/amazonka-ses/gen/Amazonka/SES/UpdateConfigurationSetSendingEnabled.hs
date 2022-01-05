{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.UpdateConfigurationSetSendingEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending for messages sent using a specific
-- configuration set in a given AWS Region. You can use this operation in
-- conjunction with Amazon CloudWatch alarms to temporarily pause email
-- sending for a configuration set when the reputation metrics for that
-- configuration set (such as your bounce on complaint rate) exceed certain
-- thresholds.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.UpdateConfigurationSetSendingEnabled
  ( -- * Creating a Request
    UpdateConfigurationSetSendingEnabled (..),
    newUpdateConfigurationSetSendingEnabled,

    -- * Request Lenses
    updateConfigurationSetSendingEnabled_configurationSetName,
    updateConfigurationSetSendingEnabled_enabled,

    -- * Destructuring the Response
    UpdateConfigurationSetSendingEnabledResponse (..),
    newUpdateConfigurationSetSendingEnabledResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to enable or disable the email sending capabilities
-- for a specific configuration set.
--
-- /See:/ 'newUpdateConfigurationSetSendingEnabled' smart constructor.
data UpdateConfigurationSetSendingEnabled = UpdateConfigurationSetSendingEnabled'
  { -- | The name of the configuration set that you want to update.
    configurationSetName :: Prelude.Text,
    -- | Describes whether email sending is enabled or disabled for the
    -- configuration set.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetSendingEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'updateConfigurationSetSendingEnabled_configurationSetName' - The name of the configuration set that you want to update.
--
-- 'enabled', 'updateConfigurationSetSendingEnabled_enabled' - Describes whether email sending is enabled or disabled for the
-- configuration set.
newUpdateConfigurationSetSendingEnabled ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'enabled'
  Prelude.Bool ->
  UpdateConfigurationSetSendingEnabled
newUpdateConfigurationSetSendingEnabled
  pConfigurationSetName_
  pEnabled_ =
    UpdateConfigurationSetSendingEnabled'
      { configurationSetName =
          pConfigurationSetName_,
        enabled = pEnabled_
      }

-- | The name of the configuration set that you want to update.
updateConfigurationSetSendingEnabled_configurationSetName :: Lens.Lens' UpdateConfigurationSetSendingEnabled Prelude.Text
updateConfigurationSetSendingEnabled_configurationSetName = Lens.lens (\UpdateConfigurationSetSendingEnabled' {configurationSetName} -> configurationSetName) (\s@UpdateConfigurationSetSendingEnabled' {} a -> s {configurationSetName = a} :: UpdateConfigurationSetSendingEnabled)

-- | Describes whether email sending is enabled or disabled for the
-- configuration set.
updateConfigurationSetSendingEnabled_enabled :: Lens.Lens' UpdateConfigurationSetSendingEnabled Prelude.Bool
updateConfigurationSetSendingEnabled_enabled = Lens.lens (\UpdateConfigurationSetSendingEnabled' {enabled} -> enabled) (\s@UpdateConfigurationSetSendingEnabled' {} a -> s {enabled = a} :: UpdateConfigurationSetSendingEnabled)

instance
  Core.AWSRequest
    UpdateConfigurationSetSendingEnabled
  where
  type
    AWSResponse UpdateConfigurationSetSendingEnabled =
      UpdateConfigurationSetSendingEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateConfigurationSetSendingEnabledResponse'

instance
  Prelude.Hashable
    UpdateConfigurationSetSendingEnabled
  where
  hashWithSalt
    _salt
    UpdateConfigurationSetSendingEnabled' {..} =
      _salt `Prelude.hashWithSalt` configurationSetName
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    UpdateConfigurationSetSendingEnabled
  where
  rnf UpdateConfigurationSetSendingEnabled' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf enabled

instance
  Core.ToHeaders
    UpdateConfigurationSetSendingEnabled
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    UpdateConfigurationSetSendingEnabled
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateConfigurationSetSendingEnabled
  where
  toQuery UpdateConfigurationSetSendingEnabled' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "UpdateConfigurationSetSendingEnabled" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName,
        "Enabled" Core.=: enabled
      ]

-- | /See:/ 'newUpdateConfigurationSetSendingEnabledResponse' smart constructor.
data UpdateConfigurationSetSendingEnabledResponse = UpdateConfigurationSetSendingEnabledResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetSendingEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateConfigurationSetSendingEnabledResponse ::
  UpdateConfigurationSetSendingEnabledResponse
newUpdateConfigurationSetSendingEnabledResponse =
  UpdateConfigurationSetSendingEnabledResponse'

instance
  Prelude.NFData
    UpdateConfigurationSetSendingEnabledResponse
  where
  rnf _ = ()
