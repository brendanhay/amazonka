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
-- Module      : Amazonka.Forecast.CreateMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a predictor monitor resource for an existing auto predictor.
-- Predictor monitoring allows you to see how your predictor\'s performance
-- changes over time. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring.html Predictor Monitoring>.
module Amazonka.Forecast.CreateMonitor
  ( -- * Creating a Request
    CreateMonitor (..),
    newCreateMonitor,

    -- * Request Lenses
    createMonitor_tags,
    createMonitor_monitorName,
    createMonitor_resourceArn,

    -- * Destructuring the Response
    CreateMonitorResponse (..),
    newCreateMonitorResponse,

    -- * Response Lenses
    createMonitorResponse_monitorArn,
    createMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMonitor' smart constructor.
data CreateMonitor = CreateMonitor'
  { -- | A list of
    -- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
    -- to apply to the monitor resource.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the monitor resource.
    monitorName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor to monitor.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMonitor_tags' - A list of
-- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
-- to apply to the monitor resource.
--
-- 'monitorName', 'createMonitor_monitorName' - The name of the monitor resource.
--
-- 'resourceArn', 'createMonitor_resourceArn' - The Amazon Resource Name (ARN) of the predictor to monitor.
newCreateMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  CreateMonitor
newCreateMonitor pMonitorName_ pResourceArn_ =
  CreateMonitor'
    { tags = Prelude.Nothing,
      monitorName = pMonitorName_,
      resourceArn = pResourceArn_
    }

-- | A list of
-- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
-- to apply to the monitor resource.
createMonitor_tags :: Lens.Lens' CreateMonitor (Prelude.Maybe [Tag])
createMonitor_tags = Lens.lens (\CreateMonitor' {tags} -> tags) (\s@CreateMonitor' {} a -> s {tags = a} :: CreateMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The name of the monitor resource.
createMonitor_monitorName :: Lens.Lens' CreateMonitor Prelude.Text
createMonitor_monitorName = Lens.lens (\CreateMonitor' {monitorName} -> monitorName) (\s@CreateMonitor' {} a -> s {monitorName = a} :: CreateMonitor)

-- | The Amazon Resource Name (ARN) of the predictor to monitor.
createMonitor_resourceArn :: Lens.Lens' CreateMonitor Prelude.Text
createMonitor_resourceArn = Lens.lens (\CreateMonitor' {resourceArn} -> resourceArn) (\s@CreateMonitor' {} a -> s {resourceArn = a} :: CreateMonitor)

instance Core.AWSRequest CreateMonitor where
  type
    AWSResponse CreateMonitor =
      CreateMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMonitorResponse'
            Prelude.<$> (x Data..?> "MonitorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMonitor where
  hashWithSalt _salt CreateMonitor' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` monitorName
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData CreateMonitor where
  rnf CreateMonitor' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders CreateMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateMonitor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMonitor where
  toJSON CreateMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("MonitorName" Data..= monitorName),
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath CreateMonitor where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMonitorResponse' smart constructor.
data CreateMonitorResponse = CreateMonitorResponse'
  { -- | The Amazon Resource Name (ARN) of the monitor resource.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorArn', 'createMonitorResponse_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource.
--
-- 'httpStatus', 'createMonitorResponse_httpStatus' - The response's http status code.
newCreateMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMonitorResponse
newCreateMonitorResponse pHttpStatus_ =
  CreateMonitorResponse'
    { monitorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the monitor resource.
createMonitorResponse_monitorArn :: Lens.Lens' CreateMonitorResponse (Prelude.Maybe Prelude.Text)
createMonitorResponse_monitorArn = Lens.lens (\CreateMonitorResponse' {monitorArn} -> monitorArn) (\s@CreateMonitorResponse' {} a -> s {monitorArn = a} :: CreateMonitorResponse)

-- | The response's http status code.
createMonitorResponse_httpStatus :: Lens.Lens' CreateMonitorResponse Prelude.Int
createMonitorResponse_httpStatus = Lens.lens (\CreateMonitorResponse' {httpStatus} -> httpStatus) (\s@CreateMonitorResponse' {} a -> s {httpStatus = a} :: CreateMonitorResponse)

instance Prelude.NFData CreateMonitorResponse where
  rnf CreateMonitorResponse' {..} =
    Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf httpStatus
