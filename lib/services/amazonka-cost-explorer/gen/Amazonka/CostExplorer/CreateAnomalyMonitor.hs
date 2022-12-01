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
-- Module      : Amazonka.CostExplorer.CreateAnomalyMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cost anomaly detection monitor with the requested type and
-- monitor specification.
module Amazonka.CostExplorer.CreateAnomalyMonitor
  ( -- * Creating a Request
    CreateAnomalyMonitor (..),
    newCreateAnomalyMonitor,

    -- * Request Lenses
    createAnomalyMonitor_resourceTags,
    createAnomalyMonitor_anomalyMonitor,

    -- * Destructuring the Response
    CreateAnomalyMonitorResponse (..),
    newCreateAnomalyMonitorResponse,

    -- * Response Lenses
    createAnomalyMonitorResponse_httpStatus,
    createAnomalyMonitorResponse_monitorArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAnomalyMonitor' smart constructor.
data CreateAnomalyMonitor = CreateAnomalyMonitor'
  { -- | An optional list of tags to associate with the specified
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalyMonitor.html AnomalyMonitor>
    -- . You can use resource tags to control access to your @monitor@ using
    -- IAM policies.
    --
    -- Each tag consists of a key and a value, and each key must be unique for
    -- the resource. The following restrictions apply to resource tags:
    --
    -- -   Although the maximum number of array members is 200, you can assign
    --     a maximum of 50 user-tags to one resource. The remaining are
    --     reserved for Amazon Web Services use
    --
    -- -   The maximum length of a key is 128 characters
    --
    -- -   The maximum length of a value is 256 characters
    --
    -- -   Keys and values can only contain alphanumeric characters, spaces,
    --     and any of the following: @_.:\/=+\@-@
    --
    -- -   Keys and values are case sensitive
    --
    -- -   Keys and values are trimmed for any leading or trailing whitespaces
    --
    -- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
    --     for Amazon Web Services use
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The cost anomaly detection monitor object that you want to create.
    anomalyMonitor :: AnomalyMonitor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTags', 'createAnomalyMonitor_resourceTags' - An optional list of tags to associate with the specified
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalyMonitor.html AnomalyMonitor>
-- . You can use resource tags to control access to your @monitor@ using
-- IAM policies.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
--
-- 'anomalyMonitor', 'createAnomalyMonitor_anomalyMonitor' - The cost anomaly detection monitor object that you want to create.
newCreateAnomalyMonitor ::
  -- | 'anomalyMonitor'
  AnomalyMonitor ->
  CreateAnomalyMonitor
newCreateAnomalyMonitor pAnomalyMonitor_ =
  CreateAnomalyMonitor'
    { resourceTags =
        Prelude.Nothing,
      anomalyMonitor = pAnomalyMonitor_
    }

-- | An optional list of tags to associate with the specified
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalyMonitor.html AnomalyMonitor>
-- . You can use resource tags to control access to your @monitor@ using
-- IAM policies.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
createAnomalyMonitor_resourceTags :: Lens.Lens' CreateAnomalyMonitor (Prelude.Maybe [ResourceTag])
createAnomalyMonitor_resourceTags = Lens.lens (\CreateAnomalyMonitor' {resourceTags} -> resourceTags) (\s@CreateAnomalyMonitor' {} a -> s {resourceTags = a} :: CreateAnomalyMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The cost anomaly detection monitor object that you want to create.
createAnomalyMonitor_anomalyMonitor :: Lens.Lens' CreateAnomalyMonitor AnomalyMonitor
createAnomalyMonitor_anomalyMonitor = Lens.lens (\CreateAnomalyMonitor' {anomalyMonitor} -> anomalyMonitor) (\s@CreateAnomalyMonitor' {} a -> s {anomalyMonitor = a} :: CreateAnomalyMonitor)

instance Core.AWSRequest CreateAnomalyMonitor where
  type
    AWSResponse CreateAnomalyMonitor =
      CreateAnomalyMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnomalyMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "MonitorArn")
      )

instance Prelude.Hashable CreateAnomalyMonitor where
  hashWithSalt _salt CreateAnomalyMonitor' {..} =
    _salt `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` anomalyMonitor

instance Prelude.NFData CreateAnomalyMonitor where
  rnf CreateAnomalyMonitor' {..} =
    Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf anomalyMonitor

instance Core.ToHeaders CreateAnomalyMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.CreateAnomalyMonitor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAnomalyMonitor where
  toJSON CreateAnomalyMonitor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceTags" Core..=) Prelude.<$> resourceTags,
            Prelude.Just
              ("AnomalyMonitor" Core..= anomalyMonitor)
          ]
      )

instance Core.ToPath CreateAnomalyMonitor where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAnomalyMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAnomalyMonitorResponse' smart constructor.
data CreateAnomalyMonitorResponse = CreateAnomalyMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of your newly created cost anomaly detection
    -- monitor.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalyMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAnomalyMonitorResponse_httpStatus' - The response's http status code.
--
-- 'monitorArn', 'createAnomalyMonitorResponse_monitorArn' - The unique identifier of your newly created cost anomaly detection
-- monitor.
newCreateAnomalyMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitorArn'
  Prelude.Text ->
  CreateAnomalyMonitorResponse
newCreateAnomalyMonitorResponse
  pHttpStatus_
  pMonitorArn_ =
    CreateAnomalyMonitorResponse'
      { httpStatus =
          pHttpStatus_,
        monitorArn = pMonitorArn_
      }

-- | The response's http status code.
createAnomalyMonitorResponse_httpStatus :: Lens.Lens' CreateAnomalyMonitorResponse Prelude.Int
createAnomalyMonitorResponse_httpStatus = Lens.lens (\CreateAnomalyMonitorResponse' {httpStatus} -> httpStatus) (\s@CreateAnomalyMonitorResponse' {} a -> s {httpStatus = a} :: CreateAnomalyMonitorResponse)

-- | The unique identifier of your newly created cost anomaly detection
-- monitor.
createAnomalyMonitorResponse_monitorArn :: Lens.Lens' CreateAnomalyMonitorResponse Prelude.Text
createAnomalyMonitorResponse_monitorArn = Lens.lens (\CreateAnomalyMonitorResponse' {monitorArn} -> monitorArn) (\s@CreateAnomalyMonitorResponse' {} a -> s {monitorArn = a} :: CreateAnomalyMonitorResponse)

instance Prelude.NFData CreateAnomalyMonitorResponse where
  rnf CreateAnomalyMonitorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitorArn
