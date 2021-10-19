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
-- Module      : Network.AWS.Connect.UpdateHoursOfOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the hours of operation.
module Network.AWS.Connect.UpdateHoursOfOperation
  ( -- * Creating a Request
    UpdateHoursOfOperation (..),
    newUpdateHoursOfOperation,

    -- * Request Lenses
    updateHoursOfOperation_config,
    updateHoursOfOperation_name,
    updateHoursOfOperation_timeZone,
    updateHoursOfOperation_description,
    updateHoursOfOperation_instanceId,
    updateHoursOfOperation_hoursOfOperationId,

    -- * Destructuring the Response
    UpdateHoursOfOperationResponse (..),
    newUpdateHoursOfOperationResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateHoursOfOperation' smart constructor.
data UpdateHoursOfOperation = UpdateHoursOfOperation'
  { -- | Configuration information of the hours of operation.
    config :: Prelude.Maybe [HoursOfOperationConfig],
    -- | The name of the hours of operation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the hours of operation.
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | The description of the hours of operation.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'name', 'updateHoursOfOperation_name' - The name of the hours of operation.
--
-- 'timeZone', 'updateHoursOfOperation_timeZone' - The time zone of the hours of operation.
--
-- 'description', 'updateHoursOfOperation_description' - The description of the hours of operation.
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
        name = Prelude.Nothing,
        timeZone = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | Configuration information of the hours of operation.
updateHoursOfOperation_config :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe [HoursOfOperationConfig])
updateHoursOfOperation_config = Lens.lens (\UpdateHoursOfOperation' {config} -> config) (\s@UpdateHoursOfOperation' {} a -> s {config = a} :: UpdateHoursOfOperation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hours of operation.
updateHoursOfOperation_name :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe Prelude.Text)
updateHoursOfOperation_name = Lens.lens (\UpdateHoursOfOperation' {name} -> name) (\s@UpdateHoursOfOperation' {} a -> s {name = a} :: UpdateHoursOfOperation)

-- | The time zone of the hours of operation.
updateHoursOfOperation_timeZone :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe Prelude.Text)
updateHoursOfOperation_timeZone = Lens.lens (\UpdateHoursOfOperation' {timeZone} -> timeZone) (\s@UpdateHoursOfOperation' {} a -> s {timeZone = a} :: UpdateHoursOfOperation)

-- | The description of the hours of operation.
updateHoursOfOperation_description :: Lens.Lens' UpdateHoursOfOperation (Prelude.Maybe Prelude.Text)
updateHoursOfOperation_description = Lens.lens (\UpdateHoursOfOperation' {description} -> description) (\s@UpdateHoursOfOperation' {} a -> s {description = a} :: UpdateHoursOfOperation)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateHoursOfOperationResponse'

instance Prelude.Hashable UpdateHoursOfOperation

instance Prelude.NFData UpdateHoursOfOperation

instance Core.ToHeaders UpdateHoursOfOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateHoursOfOperation where
  toJSON UpdateHoursOfOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Config" Core..=) Prelude.<$> config,
            ("Name" Core..=) Prelude.<$> name,
            ("TimeZone" Core..=) Prelude.<$> timeZone,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateHoursOfOperation where
  toPath UpdateHoursOfOperation' {..} =
    Prelude.mconcat
      [ "/hours-of-operations/",
        Core.toBS instanceId,
        "/",
        Core.toBS hoursOfOperationId
      ]

instance Core.ToQuery UpdateHoursOfOperation where
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
