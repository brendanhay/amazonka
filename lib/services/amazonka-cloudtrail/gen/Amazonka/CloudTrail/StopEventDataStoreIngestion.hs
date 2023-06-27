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
-- Module      : Amazonka.CloudTrail.StopEventDataStoreIngestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the ingestion of live events on an event data store specified as
-- either an ARN or the ID portion of the ARN. To stop ingestion, the event
-- data store @Status@ must be @ENABLED@ and the @eventCategory@ must be
-- @Management@, @Data@, or @ConfigurationItem@.
module Amazonka.CloudTrail.StopEventDataStoreIngestion
  ( -- * Creating a Request
    StopEventDataStoreIngestion (..),
    newStopEventDataStoreIngestion,

    -- * Request Lenses
    stopEventDataStoreIngestion_eventDataStore,

    -- * Destructuring the Response
    StopEventDataStoreIngestionResponse (..),
    newStopEventDataStoreIngestionResponse,

    -- * Response Lenses
    stopEventDataStoreIngestionResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopEventDataStoreIngestion' smart constructor.
data StopEventDataStoreIngestion = StopEventDataStoreIngestion'
  { -- | The ARN (or ID suffix of the ARN) of the event data store for which you
    -- want to stop ingestion.
    eventDataStore :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEventDataStoreIngestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataStore', 'stopEventDataStoreIngestion_eventDataStore' - The ARN (or ID suffix of the ARN) of the event data store for which you
-- want to stop ingestion.
newStopEventDataStoreIngestion ::
  -- | 'eventDataStore'
  Prelude.Text ->
  StopEventDataStoreIngestion
newStopEventDataStoreIngestion pEventDataStore_ =
  StopEventDataStoreIngestion'
    { eventDataStore =
        pEventDataStore_
    }

-- | The ARN (or ID suffix of the ARN) of the event data store for which you
-- want to stop ingestion.
stopEventDataStoreIngestion_eventDataStore :: Lens.Lens' StopEventDataStoreIngestion Prelude.Text
stopEventDataStoreIngestion_eventDataStore = Lens.lens (\StopEventDataStoreIngestion' {eventDataStore} -> eventDataStore) (\s@StopEventDataStoreIngestion' {} a -> s {eventDataStore = a} :: StopEventDataStoreIngestion)

instance Core.AWSRequest StopEventDataStoreIngestion where
  type
    AWSResponse StopEventDataStoreIngestion =
      StopEventDataStoreIngestionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopEventDataStoreIngestionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEventDataStoreIngestion where
  hashWithSalt _salt StopEventDataStoreIngestion' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData StopEventDataStoreIngestion where
  rnf StopEventDataStoreIngestion' {..} =
    Prelude.rnf eventDataStore

instance Data.ToHeaders StopEventDataStoreIngestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopEventDataStoreIngestion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopEventDataStoreIngestion where
  toJSON StopEventDataStoreIngestion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EventDataStore" Data..= eventDataStore)
          ]
      )

instance Data.ToPath StopEventDataStoreIngestion where
  toPath = Prelude.const "/"

instance Data.ToQuery StopEventDataStoreIngestion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEventDataStoreIngestionResponse' smart constructor.
data StopEventDataStoreIngestionResponse = StopEventDataStoreIngestionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEventDataStoreIngestionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopEventDataStoreIngestionResponse_httpStatus' - The response's http status code.
newStopEventDataStoreIngestionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopEventDataStoreIngestionResponse
newStopEventDataStoreIngestionResponse pHttpStatus_ =
  StopEventDataStoreIngestionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopEventDataStoreIngestionResponse_httpStatus :: Lens.Lens' StopEventDataStoreIngestionResponse Prelude.Int
stopEventDataStoreIngestionResponse_httpStatus = Lens.lens (\StopEventDataStoreIngestionResponse' {httpStatus} -> httpStatus) (\s@StopEventDataStoreIngestionResponse' {} a -> s {httpStatus = a} :: StopEventDataStoreIngestionResponse)

instance
  Prelude.NFData
    StopEventDataStoreIngestionResponse
  where
  rnf StopEventDataStoreIngestionResponse' {..} =
    Prelude.rnf httpStatus
