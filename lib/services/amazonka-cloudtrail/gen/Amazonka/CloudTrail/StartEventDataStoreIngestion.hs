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
-- Module      : Amazonka.CloudTrail.StartEventDataStoreIngestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the ingestion of live events on an event data store specified as
-- either an ARN or the ID portion of the ARN. To start ingestion, the
-- event data store @Status@ must be @STOPPED_INGESTION@ and the
-- @eventCategory@ must be @Management@, @Data@, or @ConfigurationItem@.
module Amazonka.CloudTrail.StartEventDataStoreIngestion
  ( -- * Creating a Request
    StartEventDataStoreIngestion (..),
    newStartEventDataStoreIngestion,

    -- * Request Lenses
    startEventDataStoreIngestion_eventDataStore,

    -- * Destructuring the Response
    StartEventDataStoreIngestionResponse (..),
    newStartEventDataStoreIngestionResponse,

    -- * Response Lenses
    startEventDataStoreIngestionResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartEventDataStoreIngestion' smart constructor.
data StartEventDataStoreIngestion = StartEventDataStoreIngestion'
  { -- | The ARN (or ID suffix of the ARN) of the event data store for which you
    -- want to start ingestion.
    eventDataStore :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEventDataStoreIngestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataStore', 'startEventDataStoreIngestion_eventDataStore' - The ARN (or ID suffix of the ARN) of the event data store for which you
-- want to start ingestion.
newStartEventDataStoreIngestion ::
  -- | 'eventDataStore'
  Prelude.Text ->
  StartEventDataStoreIngestion
newStartEventDataStoreIngestion pEventDataStore_ =
  StartEventDataStoreIngestion'
    { eventDataStore =
        pEventDataStore_
    }

-- | The ARN (or ID suffix of the ARN) of the event data store for which you
-- want to start ingestion.
startEventDataStoreIngestion_eventDataStore :: Lens.Lens' StartEventDataStoreIngestion Prelude.Text
startEventDataStoreIngestion_eventDataStore = Lens.lens (\StartEventDataStoreIngestion' {eventDataStore} -> eventDataStore) (\s@StartEventDataStoreIngestion' {} a -> s {eventDataStore = a} :: StartEventDataStoreIngestion)

instance Core.AWSRequest StartEventDataStoreIngestion where
  type
    AWSResponse StartEventDataStoreIngestion =
      StartEventDataStoreIngestionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartEventDataStoreIngestionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartEventDataStoreIngestion
  where
  hashWithSalt _salt StartEventDataStoreIngestion' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData StartEventDataStoreIngestion where
  rnf StartEventDataStoreIngestion' {..} =
    Prelude.rnf eventDataStore

instance Data.ToHeaders StartEventDataStoreIngestion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StartEventDataStoreIngestion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartEventDataStoreIngestion where
  toJSON StartEventDataStoreIngestion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EventDataStore" Data..= eventDataStore)
          ]
      )

instance Data.ToPath StartEventDataStoreIngestion where
  toPath = Prelude.const "/"

instance Data.ToQuery StartEventDataStoreIngestion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEventDataStoreIngestionResponse' smart constructor.
data StartEventDataStoreIngestionResponse = StartEventDataStoreIngestionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEventDataStoreIngestionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startEventDataStoreIngestionResponse_httpStatus' - The response's http status code.
newStartEventDataStoreIngestionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartEventDataStoreIngestionResponse
newStartEventDataStoreIngestionResponse pHttpStatus_ =
  StartEventDataStoreIngestionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startEventDataStoreIngestionResponse_httpStatus :: Lens.Lens' StartEventDataStoreIngestionResponse Prelude.Int
startEventDataStoreIngestionResponse_httpStatus = Lens.lens (\StartEventDataStoreIngestionResponse' {httpStatus} -> httpStatus) (\s@StartEventDataStoreIngestionResponse' {} a -> s {httpStatus = a} :: StartEventDataStoreIngestionResponse)

instance
  Prelude.NFData
    StartEventDataStoreIngestionResponse
  where
  rnf StartEventDataStoreIngestionResponse' {..} =
    Prelude.rnf httpStatus
