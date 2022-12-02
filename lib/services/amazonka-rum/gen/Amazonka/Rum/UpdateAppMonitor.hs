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
-- Module      : Amazonka.Rum.UpdateAppMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing app monitor. When you use this
-- operation, only the parts of the app monitor configuration that you
-- specify in this operation are changed. For any parameters that you omit,
-- the existing values are kept.
--
-- You can\'t use this operation to change the tags of an existing app
-- monitor. To change the tags of an existing app monitor, use
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_TagResource.html TagResource>.
--
-- To create a new app monitor, use
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_CreateAppMonitor.html CreateAppMonitor>.
--
-- After you update an app monitor, sign in to the CloudWatch RUM console
-- to get the updated JavaScript code snippet to add to your web
-- application. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-find-code-snippet.html How do I find a code snippet that I\'ve already generated?>
module Amazonka.Rum.UpdateAppMonitor
  ( -- * Creating a Request
    UpdateAppMonitor (..),
    newUpdateAppMonitor,

    -- * Request Lenses
    updateAppMonitor_domain,
    updateAppMonitor_appMonitorConfiguration,
    updateAppMonitor_cwLogEnabled,
    updateAppMonitor_customEvents,
    updateAppMonitor_name,

    -- * Destructuring the Response
    UpdateAppMonitorResponse (..),
    newUpdateAppMonitorResponse,

    -- * Response Lenses
    updateAppMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newUpdateAppMonitor' smart constructor.
data UpdateAppMonitor = UpdateAppMonitor'
  { -- | The top-level internet domain name for which your application has
    -- administrative authority.
    domain :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains much of the configuration data for the app
    -- monitor. If you are using Amazon Cognito for authorization, you must
    -- include this structure in your request, and it must include the ID of
    -- the Amazon Cognito identity pool to use for authorization. If you don\'t
    -- include @AppMonitorConfiguration@, you must set up your own
    -- authorization method. For more information, see
    -- <https://docs.aws.amazon.com/monitoring/CloudWatch-RUM-get-started-authorization.html Authorize your application to send data to Amazon Web Services>.
    appMonitorConfiguration :: Prelude.Maybe AppMonitorConfiguration,
    -- | Data collected by RUM is kept by RUM for 30 days and then deleted. This
    -- parameter specifies whether RUM sends a copy of this telemetry data to
    -- Amazon CloudWatch Logs in your account. This enables you to keep the
    -- telemetry data for more than 30 days, but it does incur Amazon
    -- CloudWatch Logs charges.
    cwLogEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether this app monitor allows the web client to define and
    -- send custom events. The default is for custom events to be @DISABLED@.
    --
    -- For more information about custom events, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
    customEvents :: Prelude.Maybe CustomEvents,
    -- | The name of the app monitor to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'updateAppMonitor_domain' - The top-level internet domain name for which your application has
-- administrative authority.
--
-- 'appMonitorConfiguration', 'updateAppMonitor_appMonitorConfiguration' - A structure that contains much of the configuration data for the app
-- monitor. If you are using Amazon Cognito for authorization, you must
-- include this structure in your request, and it must include the ID of
-- the Amazon Cognito identity pool to use for authorization. If you don\'t
-- include @AppMonitorConfiguration@, you must set up your own
-- authorization method. For more information, see
-- <https://docs.aws.amazon.com/monitoring/CloudWatch-RUM-get-started-authorization.html Authorize your application to send data to Amazon Web Services>.
--
-- 'cwLogEnabled', 'updateAppMonitor_cwLogEnabled' - Data collected by RUM is kept by RUM for 30 days and then deleted. This
-- parameter specifies whether RUM sends a copy of this telemetry data to
-- Amazon CloudWatch Logs in your account. This enables you to keep the
-- telemetry data for more than 30 days, but it does incur Amazon
-- CloudWatch Logs charges.
--
-- 'customEvents', 'updateAppMonitor_customEvents' - Specifies whether this app monitor allows the web client to define and
-- send custom events. The default is for custom events to be @DISABLED@.
--
-- For more information about custom events, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
--
-- 'name', 'updateAppMonitor_name' - The name of the app monitor to update.
newUpdateAppMonitor ::
  -- | 'name'
  Prelude.Text ->
  UpdateAppMonitor
newUpdateAppMonitor pName_ =
  UpdateAppMonitor'
    { domain = Prelude.Nothing,
      appMonitorConfiguration = Prelude.Nothing,
      cwLogEnabled = Prelude.Nothing,
      customEvents = Prelude.Nothing,
      name = pName_
    }

-- | The top-level internet domain name for which your application has
-- administrative authority.
updateAppMonitor_domain :: Lens.Lens' UpdateAppMonitor (Prelude.Maybe Prelude.Text)
updateAppMonitor_domain = Lens.lens (\UpdateAppMonitor' {domain} -> domain) (\s@UpdateAppMonitor' {} a -> s {domain = a} :: UpdateAppMonitor)

-- | A structure that contains much of the configuration data for the app
-- monitor. If you are using Amazon Cognito for authorization, you must
-- include this structure in your request, and it must include the ID of
-- the Amazon Cognito identity pool to use for authorization. If you don\'t
-- include @AppMonitorConfiguration@, you must set up your own
-- authorization method. For more information, see
-- <https://docs.aws.amazon.com/monitoring/CloudWatch-RUM-get-started-authorization.html Authorize your application to send data to Amazon Web Services>.
updateAppMonitor_appMonitorConfiguration :: Lens.Lens' UpdateAppMonitor (Prelude.Maybe AppMonitorConfiguration)
updateAppMonitor_appMonitorConfiguration = Lens.lens (\UpdateAppMonitor' {appMonitorConfiguration} -> appMonitorConfiguration) (\s@UpdateAppMonitor' {} a -> s {appMonitorConfiguration = a} :: UpdateAppMonitor)

-- | Data collected by RUM is kept by RUM for 30 days and then deleted. This
-- parameter specifies whether RUM sends a copy of this telemetry data to
-- Amazon CloudWatch Logs in your account. This enables you to keep the
-- telemetry data for more than 30 days, but it does incur Amazon
-- CloudWatch Logs charges.
updateAppMonitor_cwLogEnabled :: Lens.Lens' UpdateAppMonitor (Prelude.Maybe Prelude.Bool)
updateAppMonitor_cwLogEnabled = Lens.lens (\UpdateAppMonitor' {cwLogEnabled} -> cwLogEnabled) (\s@UpdateAppMonitor' {} a -> s {cwLogEnabled = a} :: UpdateAppMonitor)

-- | Specifies whether this app monitor allows the web client to define and
-- send custom events. The default is for custom events to be @DISABLED@.
--
-- For more information about custom events, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
updateAppMonitor_customEvents :: Lens.Lens' UpdateAppMonitor (Prelude.Maybe CustomEvents)
updateAppMonitor_customEvents = Lens.lens (\UpdateAppMonitor' {customEvents} -> customEvents) (\s@UpdateAppMonitor' {} a -> s {customEvents = a} :: UpdateAppMonitor)

-- | The name of the app monitor to update.
updateAppMonitor_name :: Lens.Lens' UpdateAppMonitor Prelude.Text
updateAppMonitor_name = Lens.lens (\UpdateAppMonitor' {name} -> name) (\s@UpdateAppMonitor' {} a -> s {name = a} :: UpdateAppMonitor)

instance Core.AWSRequest UpdateAppMonitor where
  type
    AWSResponse UpdateAppMonitor =
      UpdateAppMonitorResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAppMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAppMonitor where
  hashWithSalt _salt UpdateAppMonitor' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` appMonitorConfiguration
      `Prelude.hashWithSalt` cwLogEnabled
      `Prelude.hashWithSalt` customEvents
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateAppMonitor where
  rnf UpdateAppMonitor' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf appMonitorConfiguration
      `Prelude.seq` Prelude.rnf cwLogEnabled
      `Prelude.seq` Prelude.rnf customEvents
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateAppMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAppMonitor where
  toJSON UpdateAppMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            ("AppMonitorConfiguration" Data..=)
              Prelude.<$> appMonitorConfiguration,
            ("CwLogEnabled" Data..=) Prelude.<$> cwLogEnabled,
            ("CustomEvents" Data..=) Prelude.<$> customEvents
          ]
      )

instance Data.ToPath UpdateAppMonitor where
  toPath UpdateAppMonitor' {..} =
    Prelude.mconcat ["/appmonitor/", Data.toBS name]

instance Data.ToQuery UpdateAppMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppMonitorResponse' smart constructor.
data UpdateAppMonitorResponse = UpdateAppMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAppMonitorResponse_httpStatus' - The response's http status code.
newUpdateAppMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAppMonitorResponse
newUpdateAppMonitorResponse pHttpStatus_ =
  UpdateAppMonitorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateAppMonitorResponse_httpStatus :: Lens.Lens' UpdateAppMonitorResponse Prelude.Int
updateAppMonitorResponse_httpStatus = Lens.lens (\UpdateAppMonitorResponse' {httpStatus} -> httpStatus) (\s@UpdateAppMonitorResponse' {} a -> s {httpStatus = a} :: UpdateAppMonitorResponse)

instance Prelude.NFData UpdateAppMonitorResponse where
  rnf UpdateAppMonitorResponse' {..} =
    Prelude.rnf httpStatus
