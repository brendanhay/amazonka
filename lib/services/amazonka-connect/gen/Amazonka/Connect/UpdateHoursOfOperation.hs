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
-- Module      : Amazonka.Connect.UpdateHoursOfOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the hours of operation.
module Amazonka.Connect.UpdateHoursOfOperation
  ( -- * Creating a Request
    UpdateHoursOfOperation (..),
    newUpdateHoursOfOperation,

    -- * Request Lenses
    updateHoursOfOperation_config,
    updateHoursOfOperation_description,
    updateHoursOfOperation_name,
    updateHoursOfOperation_timeZone,
    updateHoursOfOperation_instanceId,
    updateHoursOfOperation_hoursOfOperationId,

    -- * Destructuring the Response
    UpdateHoursOfOperationResponse (..),
    newUpdateHoursOfOperationResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateHoursOfOperation' smart constructor.
data UpdateHoursOfOperation = UpdateHoursOfOperation'
  { -- | Configuration information of the hours of operation.
    config :: Prelude.Maybe [HoursOfOperationConfig],
    -- | The description of the hours of operation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the hours of operation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the hours of operation.
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the hours of operation.
    hoursOfOperationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'config', 'updateHoursOfOperation_config' - Configuration information of the hours of operation.
--
-- 'description', 'updateHoursOfOperation_description' - The description of the hours of operation.
--
-- 'name', 'updateHoursOfOperation_name' - The name of the hours of operation.
--
-- 'timeZone', 'updateHoursOfOperation_timeZone' - The time zone of the hours of operation.
--
-- 'instanceId', 'updateHoursOfOperation_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'hoursOfOperationId', 'updateHoursOfOperation_hoursOfOperationId' - The identifier of the hours of operation.
newUpdateHoursOfOperation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'hoursOfOperationId'
  Prelude.Text ->
  UpdateHoursOfOperation
newUpdateHoursOfOperation
  pInstanceId_
  pHoursOfOperationId_ =
    UpdateHoursOfOperation'
      { config = Prelude.Nothing,
        description = Prelude.Nothing,
        name = Prelude.Nothing,
        timeZone = Prelude.Nothing,
        instanceId = pInstanceId_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | Configuration information of the hours of operation.
updateHoursOfOperation_config :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe [HoursOfOperationConfig])
updateHoursOfOperation_config = Lens.lens (\UpdateHoursOfOperation' {config} -> config) (\s@UpdateHoursOfOperation' {} a -> s {config = a} :: UpdateHoursOfOperation) Prelude.. Lens.mapping Lens.coerced

-- | The description of the hours of operation.
updateHoursOfOperation_description :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe Prelude.Text)
updateHoursOfOperation_description = Lens.lens (\UpdateHoursOfOperation' {description} -> description) (\s@UpdateHoursOfOperation' {} a -> s {description = a} :: UpdateHoursOfOperation)

-- | The name of the hours of operation.
updateHoursOfOperation_name :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe Prelude.Text)
updateHoursOfOperation_name = Lens.lens (\UpdateHoursOfOperation' {name} -> name) (\s@UpdateHoursOfOperation' {} a -> s {name = a} :: UpdateHoursOfOperation)

-- | The time zone of the hours of operation.
updateHoursOfOperation_timeZone :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe Prelude.Text)
updateHoursOfOperation_timeZone = Lens.lens (\UpdateHoursOfOperation' {timeZone} -> timeZone) (\s@UpdateHoursOfOperation' {} a -> s {timeZone = a} :: UpdateHoursOfOperation)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateHoursOfOperation_instanceId :: Lens.Lens' UpdateHoursOfOperation Prelude.Text
updateHoursOfOperation_instanceId = Lens.lens (\UpdateHoursOfOperation' {instanceId} -> instanceId) (\s@UpdateHoursOfOperation' {} a -> s {instanceId = a} :: UpdateHoursOfOperation)

-- | The identifier of the hours of operation.
updateHoursOfOperation_hoursOfOperationId :: Lens.Lens' UpdateHoursOfOperation Prelude.Text
updateHoursOfOperation_hoursOfOperationId = Lens.lens (\UpdateHoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@UpdateHoursOfOperation' {} a -> s {hoursOfOperationId = a} :: UpdateHoursOfOperation)

instance Core.AWSRequest UpdateHoursOfOperation where
  type
    AWSResponse UpdateHoursOfOperation =
      UpdateHoursOfOperationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateHoursOfOperationResponse'

instance Prelude.Hashable UpdateHoursOfOperation where
  hashWithSalt _salt UpdateHoursOfOperation' {..} =
    _salt
      `Prelude.hashWithSalt` config
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timeZone
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` hoursOfOperationId

instance Prelude.NFData UpdateHoursOfOperation where
  rnf UpdateHoursOfOperation' {..} =
    Prelude.rnf config
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf hoursOfOperationId

instance Data.ToHeaders UpdateHoursOfOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateHoursOfOperation where
  toJSON UpdateHoursOfOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Config" Data..=) Prelude.<$> config,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("TimeZone" Data..=) Prelude.<$> timeZone
          ]
      )

instance Data.ToPath UpdateHoursOfOperation where
  toPath UpdateHoursOfOperation' {..} =
    Prelude.mconcat
      [ "/hours-of-operations/",
        Data.toBS instanceId,
        "/",
        Data.toBS hoursOfOperationId
      ]

instance Data.ToQuery UpdateHoursOfOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHoursOfOperationResponse' smart constructor.
data UpdateHoursOfOperationResponse = UpdateHoursOfOperationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHoursOfOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateHoursOfOperationResponse ::
  UpdateHoursOfOperationResponse
newUpdateHoursOfOperationResponse =
  UpdateHoursOfOperationResponse'

instance
  Prelude.NFData
    UpdateHoursOfOperationResponse
  where
  rnf _ = ()
