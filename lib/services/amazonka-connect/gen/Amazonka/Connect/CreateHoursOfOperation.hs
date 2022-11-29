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
-- Module      : Amazonka.Connect.CreateHoursOfOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Creates hours of operation.
module Amazonka.Connect.CreateHoursOfOperation
  ( -- * Creating a Request
    CreateHoursOfOperation (..),
    newCreateHoursOfOperation,

    -- * Request Lenses
    createHoursOfOperation_tags,
    createHoursOfOperation_description,
    createHoursOfOperation_instanceId,
    createHoursOfOperation_name,
    createHoursOfOperation_timeZone,
    createHoursOfOperation_config,

    -- * Destructuring the Response
    CreateHoursOfOperationResponse (..),
    newCreateHoursOfOperationResponse,

    -- * Response Lenses
    createHoursOfOperationResponse_hoursOfOperationArn,
    createHoursOfOperationResponse_hoursOfOperationId,
    createHoursOfOperationResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateHoursOfOperation' smart constructor.
data CreateHoursOfOperation = CreateHoursOfOperation'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the hours of operation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the hours of operation.
    name :: Prelude.Text,
    -- | The time zone of the hours of operation.
    timeZone :: Prelude.Text,
    -- | Configuration information for the hours of operation: day, start time,
    -- and end time.
    config :: [HoursOfOperationConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createHoursOfOperation_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'description', 'createHoursOfOperation_description' - The description of the hours of operation.
--
-- 'instanceId', 'createHoursOfOperation_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'name', 'createHoursOfOperation_name' - The name of the hours of operation.
--
-- 'timeZone', 'createHoursOfOperation_timeZone' - The time zone of the hours of operation.
--
-- 'config', 'createHoursOfOperation_config' - Configuration information for the hours of operation: day, start time,
-- and end time.
newCreateHoursOfOperation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'timeZone'
  Prelude.Text ->
  CreateHoursOfOperation
newCreateHoursOfOperation
  pInstanceId_
  pName_
  pTimeZone_ =
    CreateHoursOfOperation'
      { tags = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        name = pName_,
        timeZone = pTimeZone_,
        config = Prelude.mempty
      }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createHoursOfOperation_tags :: Lens.Lens' CreateHoursOfOperation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createHoursOfOperation_tags = Lens.lens (\CreateHoursOfOperation' {tags} -> tags) (\s@CreateHoursOfOperation' {} a -> s {tags = a} :: CreateHoursOfOperation) Prelude.. Lens.mapping Lens.coerced

-- | The description of the hours of operation.
createHoursOfOperation_description :: Lens.Lens' CreateHoursOfOperation (Prelude.Maybe Prelude.Text)
createHoursOfOperation_description = Lens.lens (\CreateHoursOfOperation' {description} -> description) (\s@CreateHoursOfOperation' {} a -> s {description = a} :: CreateHoursOfOperation)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createHoursOfOperation_instanceId :: Lens.Lens' CreateHoursOfOperation Prelude.Text
createHoursOfOperation_instanceId = Lens.lens (\CreateHoursOfOperation' {instanceId} -> instanceId) (\s@CreateHoursOfOperation' {} a -> s {instanceId = a} :: CreateHoursOfOperation)

-- | The name of the hours of operation.
createHoursOfOperation_name :: Lens.Lens' CreateHoursOfOperation Prelude.Text
createHoursOfOperation_name = Lens.lens (\CreateHoursOfOperation' {name} -> name) (\s@CreateHoursOfOperation' {} a -> s {name = a} :: CreateHoursOfOperation)

-- | The time zone of the hours of operation.
createHoursOfOperation_timeZone :: Lens.Lens' CreateHoursOfOperation Prelude.Text
createHoursOfOperation_timeZone = Lens.lens (\CreateHoursOfOperation' {timeZone} -> timeZone) (\s@CreateHoursOfOperation' {} a -> s {timeZone = a} :: CreateHoursOfOperation)

-- | Configuration information for the hours of operation: day, start time,
-- and end time.
createHoursOfOperation_config :: Lens.Lens' CreateHoursOfOperation [HoursOfOperationConfig]
createHoursOfOperation_config = Lens.lens (\CreateHoursOfOperation' {config} -> config) (\s@CreateHoursOfOperation' {} a -> s {config = a} :: CreateHoursOfOperation) Prelude.. Lens.coerced

instance Core.AWSRequest CreateHoursOfOperation where
  type
    AWSResponse CreateHoursOfOperation =
      CreateHoursOfOperationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHoursOfOperationResponse'
            Prelude.<$> (x Core..?> "HoursOfOperationArn")
            Prelude.<*> (x Core..?> "HoursOfOperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHoursOfOperation where
  hashWithSalt _salt CreateHoursOfOperation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timeZone
      `Prelude.hashWithSalt` config

instance Prelude.NFData CreateHoursOfOperation where
  rnf CreateHoursOfOperation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf config

instance Core.ToHeaders CreateHoursOfOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateHoursOfOperation where
  toJSON CreateHoursOfOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("TimeZone" Core..= timeZone),
            Prelude.Just ("Config" Core..= config)
          ]
      )

instance Core.ToPath CreateHoursOfOperation where
  toPath CreateHoursOfOperation' {..} =
    Prelude.mconcat
      ["/hours-of-operations/", Core.toBS instanceId]

instance Core.ToQuery CreateHoursOfOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHoursOfOperationResponse' smart constructor.
data CreateHoursOfOperationResponse = CreateHoursOfOperationResponse'
  { -- | The Amazon Resource Name (ARN) for the hours of operation.
    hoursOfOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHoursOfOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hoursOfOperationArn', 'createHoursOfOperationResponse_hoursOfOperationArn' - The Amazon Resource Name (ARN) for the hours of operation.
--
-- 'hoursOfOperationId', 'createHoursOfOperationResponse_hoursOfOperationId' - The identifier for the hours of operation.
--
-- 'httpStatus', 'createHoursOfOperationResponse_httpStatus' - The response's http status code.
newCreateHoursOfOperationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHoursOfOperationResponse
newCreateHoursOfOperationResponse pHttpStatus_ =
  CreateHoursOfOperationResponse'
    { hoursOfOperationArn =
        Prelude.Nothing,
      hoursOfOperationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the hours of operation.
createHoursOfOperationResponse_hoursOfOperationArn :: Lens.Lens' CreateHoursOfOperationResponse (Prelude.Maybe Prelude.Text)
createHoursOfOperationResponse_hoursOfOperationArn = Lens.lens (\CreateHoursOfOperationResponse' {hoursOfOperationArn} -> hoursOfOperationArn) (\s@CreateHoursOfOperationResponse' {} a -> s {hoursOfOperationArn = a} :: CreateHoursOfOperationResponse)

-- | The identifier for the hours of operation.
createHoursOfOperationResponse_hoursOfOperationId :: Lens.Lens' CreateHoursOfOperationResponse (Prelude.Maybe Prelude.Text)
createHoursOfOperationResponse_hoursOfOperationId = Lens.lens (\CreateHoursOfOperationResponse' {hoursOfOperationId} -> hoursOfOperationId) (\s@CreateHoursOfOperationResponse' {} a -> s {hoursOfOperationId = a} :: CreateHoursOfOperationResponse)

-- | The response's http status code.
createHoursOfOperationResponse_httpStatus :: Lens.Lens' CreateHoursOfOperationResponse Prelude.Int
createHoursOfOperationResponse_httpStatus = Lens.lens (\CreateHoursOfOperationResponse' {httpStatus} -> httpStatus) (\s@CreateHoursOfOperationResponse' {} a -> s {httpStatus = a} :: CreateHoursOfOperationResponse)

instance
  Prelude.NFData
    CreateHoursOfOperationResponse
  where
  rnf CreateHoursOfOperationResponse' {..} =
    Prelude.rnf hoursOfOperationArn
      `Prelude.seq` Prelude.rnf hoursOfOperationId
      `Prelude.seq` Prelude.rnf httpStatus
