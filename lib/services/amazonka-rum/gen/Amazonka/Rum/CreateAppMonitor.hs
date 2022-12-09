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
-- Module      : Amazonka.Rum.CreateAppMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Amazon CloudWatch RUM app monitor, which collects telemetry
-- data from your application and sends that data to RUM. The data includes
-- performance and reliability information such as page load time,
-- client-side errors, and user behavior.
--
-- You use this operation only to create a new app monitor. To update an
-- existing app monitor, use
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_UpdateAppMonitor.html UpdateAppMonitor>
-- instead.
--
-- After you create an app monitor, sign in to the CloudWatch RUM console
-- to get the JavaScript code snippet to add to your web application. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-find-code-snippet.html How do I find a code snippet that I\'ve already generated?>
module Amazonka.Rum.CreateAppMonitor
  ( -- * Creating a Request
    CreateAppMonitor (..),
    newCreateAppMonitor,

    -- * Request Lenses
    createAppMonitor_appMonitorConfiguration,
    createAppMonitor_customEvents,
    createAppMonitor_cwLogEnabled,
    createAppMonitor_tags,
    createAppMonitor_domain,
    createAppMonitor_name,

    -- * Destructuring the Response
    CreateAppMonitorResponse (..),
    newCreateAppMonitorResponse,

    -- * Response Lenses
    createAppMonitorResponse_id,
    createAppMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newCreateAppMonitor' smart constructor.
data CreateAppMonitor = CreateAppMonitor'
  { -- | A structure that contains much of the configuration data for the app
    -- monitor. If you are using Amazon Cognito for authorization, you must
    -- include this structure in your request, and it must include the ID of
    -- the Amazon Cognito identity pool to use for authorization. If you don\'t
    -- include @AppMonitorConfiguration@, you must set up your own
    -- authorization method. For more information, see
    -- <https://docs.aws.amazon.com/monitoring/CloudWatch-RUM-get-started-authorization.html Authorize your application to send data to Amazon Web Services>.
    --
    -- If you omit this argument, the sample rate used for RUM is set to 10% of
    -- the user sessions.
    appMonitorConfiguration :: Prelude.Maybe AppMonitorConfiguration,
    -- | Specifies whether this app monitor allows the web client to define and
    -- send custom events. If you omit this parameter, custom events are
    -- @DISABLED@.
    --
    -- For more information about custom events, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
    customEvents :: Prelude.Maybe CustomEvents,
    -- | Data collected by RUM is kept by RUM for 30 days and then deleted. This
    -- parameter specifies whether RUM sends a copy of this telemetry data to
    -- Amazon CloudWatch Logs in your account. This enables you to keep the
    -- telemetry data for more than 30 days, but it does incur Amazon
    -- CloudWatch Logs charges.
    --
    -- If you omit this parameter, the default is @false@.
    cwLogEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Assigns one or more tags (key-value pairs) to the app monitor.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- Tags don\'t have any semantic meaning to Amazon Web Services and are
    -- interpreted strictly as strings of characters.
    --
    -- You can associate as many as 50 tags with an app monitor.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The top-level internet domain name for which your application has
    -- administrative authority.
    domain :: Prelude.Text,
    -- | A name for the app monitor.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appMonitorConfiguration', 'createAppMonitor_appMonitorConfiguration' - A structure that contains much of the configuration data for the app
-- monitor. If you are using Amazon Cognito for authorization, you must
-- include this structure in your request, and it must include the ID of
-- the Amazon Cognito identity pool to use for authorization. If you don\'t
-- include @AppMonitorConfiguration@, you must set up your own
-- authorization method. For more information, see
-- <https://docs.aws.amazon.com/monitoring/CloudWatch-RUM-get-started-authorization.html Authorize your application to send data to Amazon Web Services>.
--
-- If you omit this argument, the sample rate used for RUM is set to 10% of
-- the user sessions.
--
-- 'customEvents', 'createAppMonitor_customEvents' - Specifies whether this app monitor allows the web client to define and
-- send custom events. If you omit this parameter, custom events are
-- @DISABLED@.
--
-- For more information about custom events, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
--
-- 'cwLogEnabled', 'createAppMonitor_cwLogEnabled' - Data collected by RUM is kept by RUM for 30 days and then deleted. This
-- parameter specifies whether RUM sends a copy of this telemetry data to
-- Amazon CloudWatch Logs in your account. This enables you to keep the
-- telemetry data for more than 30 days, but it does incur Amazon
-- CloudWatch Logs charges.
--
-- If you omit this parameter, the default is @false@.
--
-- 'tags', 'createAppMonitor_tags' - Assigns one or more tags (key-value pairs) to the app monitor.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- You can associate as many as 50 tags with an app monitor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
--
-- 'domain', 'createAppMonitor_domain' - The top-level internet domain name for which your application has
-- administrative authority.
--
-- 'name', 'createAppMonitor_name' - A name for the app monitor.
newCreateAppMonitor ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateAppMonitor
newCreateAppMonitor pDomain_ pName_ =
  CreateAppMonitor'
    { appMonitorConfiguration =
        Prelude.Nothing,
      customEvents = Prelude.Nothing,
      cwLogEnabled = Prelude.Nothing,
      tags = Prelude.Nothing,
      domain = pDomain_,
      name = pName_
    }

-- | A structure that contains much of the configuration data for the app
-- monitor. If you are using Amazon Cognito for authorization, you must
-- include this structure in your request, and it must include the ID of
-- the Amazon Cognito identity pool to use for authorization. If you don\'t
-- include @AppMonitorConfiguration@, you must set up your own
-- authorization method. For more information, see
-- <https://docs.aws.amazon.com/monitoring/CloudWatch-RUM-get-started-authorization.html Authorize your application to send data to Amazon Web Services>.
--
-- If you omit this argument, the sample rate used for RUM is set to 10% of
-- the user sessions.
createAppMonitor_appMonitorConfiguration :: Lens.Lens' CreateAppMonitor (Prelude.Maybe AppMonitorConfiguration)
createAppMonitor_appMonitorConfiguration = Lens.lens (\CreateAppMonitor' {appMonitorConfiguration} -> appMonitorConfiguration) (\s@CreateAppMonitor' {} a -> s {appMonitorConfiguration = a} :: CreateAppMonitor)

-- | Specifies whether this app monitor allows the web client to define and
-- send custom events. If you omit this parameter, custom events are
-- @DISABLED@.
--
-- For more information about custom events, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
createAppMonitor_customEvents :: Lens.Lens' CreateAppMonitor (Prelude.Maybe CustomEvents)
createAppMonitor_customEvents = Lens.lens (\CreateAppMonitor' {customEvents} -> customEvents) (\s@CreateAppMonitor' {} a -> s {customEvents = a} :: CreateAppMonitor)

-- | Data collected by RUM is kept by RUM for 30 days and then deleted. This
-- parameter specifies whether RUM sends a copy of this telemetry data to
-- Amazon CloudWatch Logs in your account. This enables you to keep the
-- telemetry data for more than 30 days, but it does incur Amazon
-- CloudWatch Logs charges.
--
-- If you omit this parameter, the default is @false@.
createAppMonitor_cwLogEnabled :: Lens.Lens' CreateAppMonitor (Prelude.Maybe Prelude.Bool)
createAppMonitor_cwLogEnabled = Lens.lens (\CreateAppMonitor' {cwLogEnabled} -> cwLogEnabled) (\s@CreateAppMonitor' {} a -> s {cwLogEnabled = a} :: CreateAppMonitor)

-- | Assigns one or more tags (key-value pairs) to the app monitor.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- You can associate as many as 50 tags with an app monitor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
createAppMonitor_tags :: Lens.Lens' CreateAppMonitor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAppMonitor_tags = Lens.lens (\CreateAppMonitor' {tags} -> tags) (\s@CreateAppMonitor' {} a -> s {tags = a} :: CreateAppMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The top-level internet domain name for which your application has
-- administrative authority.
createAppMonitor_domain :: Lens.Lens' CreateAppMonitor Prelude.Text
createAppMonitor_domain = Lens.lens (\CreateAppMonitor' {domain} -> domain) (\s@CreateAppMonitor' {} a -> s {domain = a} :: CreateAppMonitor)

-- | A name for the app monitor.
createAppMonitor_name :: Lens.Lens' CreateAppMonitor Prelude.Text
createAppMonitor_name = Lens.lens (\CreateAppMonitor' {name} -> name) (\s@CreateAppMonitor' {} a -> s {name = a} :: CreateAppMonitor)

instance Core.AWSRequest CreateAppMonitor where
  type
    AWSResponse CreateAppMonitor =
      CreateAppMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppMonitorResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAppMonitor where
  hashWithSalt _salt CreateAppMonitor' {..} =
    _salt
      `Prelude.hashWithSalt` appMonitorConfiguration
      `Prelude.hashWithSalt` customEvents
      `Prelude.hashWithSalt` cwLogEnabled
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateAppMonitor where
  rnf CreateAppMonitor' {..} =
    Prelude.rnf appMonitorConfiguration
      `Prelude.seq` Prelude.rnf customEvents
      `Prelude.seq` Prelude.rnf cwLogEnabled
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateAppMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAppMonitor where
  toJSON CreateAppMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppMonitorConfiguration" Data..=)
              Prelude.<$> appMonitorConfiguration,
            ("CustomEvents" Data..=) Prelude.<$> customEvents,
            ("CwLogEnabled" Data..=) Prelude.<$> cwLogEnabled,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Domain" Data..= domain),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateAppMonitor where
  toPath = Prelude.const "/appmonitor"

instance Data.ToQuery CreateAppMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppMonitorResponse' smart constructor.
data CreateAppMonitorResponse = CreateAppMonitorResponse'
  { -- | The unique ID of the new app monitor.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createAppMonitorResponse_id' - The unique ID of the new app monitor.
--
-- 'httpStatus', 'createAppMonitorResponse_httpStatus' - The response's http status code.
newCreateAppMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppMonitorResponse
newCreateAppMonitorResponse pHttpStatus_ =
  CreateAppMonitorResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the new app monitor.
createAppMonitorResponse_id :: Lens.Lens' CreateAppMonitorResponse (Prelude.Maybe Prelude.Text)
createAppMonitorResponse_id = Lens.lens (\CreateAppMonitorResponse' {id} -> id) (\s@CreateAppMonitorResponse' {} a -> s {id = a} :: CreateAppMonitorResponse)

-- | The response's http status code.
createAppMonitorResponse_httpStatus :: Lens.Lens' CreateAppMonitorResponse Prelude.Int
createAppMonitorResponse_httpStatus = Lens.lens (\CreateAppMonitorResponse' {httpStatus} -> httpStatus) (\s@CreateAppMonitorResponse' {} a -> s {httpStatus = a} :: CreateAppMonitorResponse)

instance Prelude.NFData CreateAppMonitorResponse where
  rnf CreateAppMonitorResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
