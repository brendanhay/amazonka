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
-- Module      : Amazonka.OpenSearch.StartServiceSoftwareUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon OpenSearch Service
-- domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/service-software.html Service software updates in Amazon OpenSearch Service>.
module Amazonka.OpenSearch.StartServiceSoftwareUpdate
  ( -- * Creating a Request
    StartServiceSoftwareUpdate (..),
    newStartServiceSoftwareUpdate,

    -- * Request Lenses
    startServiceSoftwareUpdate_desiredStartTime,
    startServiceSoftwareUpdate_scheduleAt,
    startServiceSoftwareUpdate_domainName,

    -- * Destructuring the Response
    StartServiceSoftwareUpdateResponse (..),
    newStartServiceSoftwareUpdateResponse,

    -- * Response Lenses
    startServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startServiceSoftwareUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @StartServiceSoftwareUpdate@
-- operation.
--
-- /See:/ 'newStartServiceSoftwareUpdate' smart constructor.
data StartServiceSoftwareUpdate = StartServiceSoftwareUpdate'
  { -- | The Epoch timestamp when you want the service software update to start.
    -- You only need to specify this parameter if you set @ScheduleAt@ to
    -- @TIMESTAMP@.
    desiredStartTime :: Prelude.Maybe Prelude.Integer,
    -- | When to start the service software update.
    --
    -- -   @NOW@ - Immediately schedules the update to happen in the current
    --     hour if there\'s capacity available.
    --
    -- -   @TIMESTAMP@ - Lets you specify a custom date and time to apply the
    --     update. If you specify this value, you must also provide a value for
    --     @DesiredStartTime@.
    --
    -- -   @OFF_PEAK_WINDOW@ - Marks the update to be picked up during an
    --     upcoming off-peak window. There\'s no guarantee that the update will
    --     happen during the next immediate window. Depending on capacity, it
    --     might happen in subsequent days.
    --
    -- Default: @NOW@ if you don\'t specify a value for @DesiredStartTime@, and
    -- @TIMESTAMP@ if you do.
    scheduleAt :: Prelude.Maybe ScheduleAt,
    -- | The name of the domain that you want to update to the latest service
    -- software.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartServiceSoftwareUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredStartTime', 'startServiceSoftwareUpdate_desiredStartTime' - The Epoch timestamp when you want the service software update to start.
-- You only need to specify this parameter if you set @ScheduleAt@ to
-- @TIMESTAMP@.
--
-- 'scheduleAt', 'startServiceSoftwareUpdate_scheduleAt' - When to start the service software update.
--
-- -   @NOW@ - Immediately schedules the update to happen in the current
--     hour if there\'s capacity available.
--
-- -   @TIMESTAMP@ - Lets you specify a custom date and time to apply the
--     update. If you specify this value, you must also provide a value for
--     @DesiredStartTime@.
--
-- -   @OFF_PEAK_WINDOW@ - Marks the update to be picked up during an
--     upcoming off-peak window. There\'s no guarantee that the update will
--     happen during the next immediate window. Depending on capacity, it
--     might happen in subsequent days.
--
-- Default: @NOW@ if you don\'t specify a value for @DesiredStartTime@, and
-- @TIMESTAMP@ if you do.
--
-- 'domainName', 'startServiceSoftwareUpdate_domainName' - The name of the domain that you want to update to the latest service
-- software.
newStartServiceSoftwareUpdate ::
  -- | 'domainName'
  Prelude.Text ->
  StartServiceSoftwareUpdate
newStartServiceSoftwareUpdate pDomainName_ =
  StartServiceSoftwareUpdate'
    { desiredStartTime =
        Prelude.Nothing,
      scheduleAt = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The Epoch timestamp when you want the service software update to start.
-- You only need to specify this parameter if you set @ScheduleAt@ to
-- @TIMESTAMP@.
startServiceSoftwareUpdate_desiredStartTime :: Lens.Lens' StartServiceSoftwareUpdate (Prelude.Maybe Prelude.Integer)
startServiceSoftwareUpdate_desiredStartTime = Lens.lens (\StartServiceSoftwareUpdate' {desiredStartTime} -> desiredStartTime) (\s@StartServiceSoftwareUpdate' {} a -> s {desiredStartTime = a} :: StartServiceSoftwareUpdate)

-- | When to start the service software update.
--
-- -   @NOW@ - Immediately schedules the update to happen in the current
--     hour if there\'s capacity available.
--
-- -   @TIMESTAMP@ - Lets you specify a custom date and time to apply the
--     update. If you specify this value, you must also provide a value for
--     @DesiredStartTime@.
--
-- -   @OFF_PEAK_WINDOW@ - Marks the update to be picked up during an
--     upcoming off-peak window. There\'s no guarantee that the update will
--     happen during the next immediate window. Depending on capacity, it
--     might happen in subsequent days.
--
-- Default: @NOW@ if you don\'t specify a value for @DesiredStartTime@, and
-- @TIMESTAMP@ if you do.
startServiceSoftwareUpdate_scheduleAt :: Lens.Lens' StartServiceSoftwareUpdate (Prelude.Maybe ScheduleAt)
startServiceSoftwareUpdate_scheduleAt = Lens.lens (\StartServiceSoftwareUpdate' {scheduleAt} -> scheduleAt) (\s@StartServiceSoftwareUpdate' {} a -> s {scheduleAt = a} :: StartServiceSoftwareUpdate)

-- | The name of the domain that you want to update to the latest service
-- software.
startServiceSoftwareUpdate_domainName :: Lens.Lens' StartServiceSoftwareUpdate Prelude.Text
startServiceSoftwareUpdate_domainName = Lens.lens (\StartServiceSoftwareUpdate' {domainName} -> domainName) (\s@StartServiceSoftwareUpdate' {} a -> s {domainName = a} :: StartServiceSoftwareUpdate)

instance Core.AWSRequest StartServiceSoftwareUpdate where
  type
    AWSResponse StartServiceSoftwareUpdate =
      StartServiceSoftwareUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartServiceSoftwareUpdateResponse'
            Prelude.<$> (x Data..?> "ServiceSoftwareOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartServiceSoftwareUpdate where
  hashWithSalt _salt StartServiceSoftwareUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` desiredStartTime
      `Prelude.hashWithSalt` scheduleAt
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData StartServiceSoftwareUpdate where
  rnf StartServiceSoftwareUpdate' {..} =
    Prelude.rnf desiredStartTime
      `Prelude.seq` Prelude.rnf scheduleAt
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders StartServiceSoftwareUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartServiceSoftwareUpdate where
  toJSON StartServiceSoftwareUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredStartTime" Data..=)
              Prelude.<$> desiredStartTime,
            ("ScheduleAt" Data..=) Prelude.<$> scheduleAt,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath StartServiceSoftwareUpdate where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/serviceSoftwareUpdate/start"

instance Data.ToQuery StartServiceSoftwareUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @StartServiceSoftwareUpdate@ operation.
-- Contains the status of the update.
--
-- /See:/ 'newStartServiceSoftwareUpdateResponse' smart constructor.
data StartServiceSoftwareUpdateResponse = StartServiceSoftwareUpdateResponse'
  { -- | The current status of the OpenSearch Service software update.
    serviceSoftwareOptions :: Prelude.Maybe ServiceSoftwareOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartServiceSoftwareUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSoftwareOptions', 'startServiceSoftwareUpdateResponse_serviceSoftwareOptions' - The current status of the OpenSearch Service software update.
--
-- 'httpStatus', 'startServiceSoftwareUpdateResponse_httpStatus' - The response's http status code.
newStartServiceSoftwareUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartServiceSoftwareUpdateResponse
newStartServiceSoftwareUpdateResponse pHttpStatus_ =
  StartServiceSoftwareUpdateResponse'
    { serviceSoftwareOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the OpenSearch Service software update.
startServiceSoftwareUpdateResponse_serviceSoftwareOptions :: Lens.Lens' StartServiceSoftwareUpdateResponse (Prelude.Maybe ServiceSoftwareOptions)
startServiceSoftwareUpdateResponse_serviceSoftwareOptions = Lens.lens (\StartServiceSoftwareUpdateResponse' {serviceSoftwareOptions} -> serviceSoftwareOptions) (\s@StartServiceSoftwareUpdateResponse' {} a -> s {serviceSoftwareOptions = a} :: StartServiceSoftwareUpdateResponse)

-- | The response's http status code.
startServiceSoftwareUpdateResponse_httpStatus :: Lens.Lens' StartServiceSoftwareUpdateResponse Prelude.Int
startServiceSoftwareUpdateResponse_httpStatus = Lens.lens (\StartServiceSoftwareUpdateResponse' {httpStatus} -> httpStatus) (\s@StartServiceSoftwareUpdateResponse' {} a -> s {httpStatus = a} :: StartServiceSoftwareUpdateResponse)

instance
  Prelude.NFData
    StartServiceSoftwareUpdateResponse
  where
  rnf StartServiceSoftwareUpdateResponse' {..} =
    Prelude.rnf serviceSoftwareOptions
      `Prelude.seq` Prelude.rnf httpStatus
