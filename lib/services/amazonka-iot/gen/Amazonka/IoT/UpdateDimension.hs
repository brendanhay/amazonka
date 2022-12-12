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
-- Module      : Amazonka.IoT.UpdateDimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for a dimension. You cannot change the type of a
-- dimension after it is created (you can delete it and recreate it).
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateDimension>
-- action.
module Amazonka.IoT.UpdateDimension
  ( -- * Creating a Request
    UpdateDimension (..),
    newUpdateDimension,

    -- * Request Lenses
    updateDimension_name,
    updateDimension_stringValues,

    -- * Destructuring the Response
    UpdateDimensionResponse (..),
    newUpdateDimensionResponse,

    -- * Response Lenses
    updateDimensionResponse_arn,
    updateDimensionResponse_creationDate,
    updateDimensionResponse_lastModifiedDate,
    updateDimensionResponse_name,
    updateDimensionResponse_stringValues,
    updateDimensionResponse_type,
    updateDimensionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDimension' smart constructor.
data UpdateDimension = UpdateDimension'
  { -- | A unique identifier for the dimension. Choose something that describes
    -- the type and value to make it easy to remember what it does.
    name :: Prelude.Text,
    -- | Specifies the value or list of values for the dimension. For
    -- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
    -- topic (for example, \"admin\/#\").
    stringValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateDimension_name' - A unique identifier for the dimension. Choose something that describes
-- the type and value to make it easy to remember what it does.
--
-- 'stringValues', 'updateDimension_stringValues' - Specifies the value or list of values for the dimension. For
-- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
-- topic (for example, \"admin\/#\").
newUpdateDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'stringValues'
  Prelude.NonEmpty Prelude.Text ->
  UpdateDimension
newUpdateDimension pName_ pStringValues_ =
  UpdateDimension'
    { name = pName_,
      stringValues = Lens.coerced Lens.# pStringValues_
    }

-- | A unique identifier for the dimension. Choose something that describes
-- the type and value to make it easy to remember what it does.
updateDimension_name :: Lens.Lens' UpdateDimension Prelude.Text
updateDimension_name = Lens.lens (\UpdateDimension' {name} -> name) (\s@UpdateDimension' {} a -> s {name = a} :: UpdateDimension)

-- | Specifies the value or list of values for the dimension. For
-- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
-- topic (for example, \"admin\/#\").
updateDimension_stringValues :: Lens.Lens' UpdateDimension (Prelude.NonEmpty Prelude.Text)
updateDimension_stringValues = Lens.lens (\UpdateDimension' {stringValues} -> stringValues) (\s@UpdateDimension' {} a -> s {stringValues = a} :: UpdateDimension) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDimension where
  type
    AWSResponse UpdateDimension =
      UpdateDimensionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDimensionResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "stringValues")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDimension where
  hashWithSalt _salt UpdateDimension' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stringValues

instance Prelude.NFData UpdateDimension where
  rnf UpdateDimension' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf stringValues

instance Data.ToHeaders UpdateDimension where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateDimension where
  toJSON UpdateDimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("stringValues" Data..= stringValues)]
      )

instance Data.ToPath UpdateDimension where
  toPath UpdateDimension' {..} =
    Prelude.mconcat ["/dimensions/", Data.toBS name]

instance Data.ToQuery UpdateDimension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDimensionResponse' smart constructor.
data UpdateDimensionResponse = UpdateDimensionResponse'
  { -- | The Amazon Resource Name (ARN)of the created dimension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in milliseconds since epoch, when the dimension was
    -- initially created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time, in milliseconds since epoch, when the dimension was
    -- most recently updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the dimension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value or list of values used to scope the dimension. For example,
    -- for topic filters, this is the pattern used to match the MQTT topic
    -- name.
    stringValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of the dimension.
    type' :: Prelude.Maybe DimensionType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDimensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateDimensionResponse_arn' - The Amazon Resource Name (ARN)of the created dimension.
--
-- 'creationDate', 'updateDimensionResponse_creationDate' - The date and time, in milliseconds since epoch, when the dimension was
-- initially created.
--
-- 'lastModifiedDate', 'updateDimensionResponse_lastModifiedDate' - The date and time, in milliseconds since epoch, when the dimension was
-- most recently updated.
--
-- 'name', 'updateDimensionResponse_name' - A unique identifier for the dimension.
--
-- 'stringValues', 'updateDimensionResponse_stringValues' - The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
--
-- 'type'', 'updateDimensionResponse_type' - The type of the dimension.
--
-- 'httpStatus', 'updateDimensionResponse_httpStatus' - The response's http status code.
newUpdateDimensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDimensionResponse
newUpdateDimensionResponse pHttpStatus_ =
  UpdateDimensionResponse'
    { arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      stringValues = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN)of the created dimension.
updateDimensionResponse_arn :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.Text)
updateDimensionResponse_arn = Lens.lens (\UpdateDimensionResponse' {arn} -> arn) (\s@UpdateDimensionResponse' {} a -> s {arn = a} :: UpdateDimensionResponse)

-- | The date and time, in milliseconds since epoch, when the dimension was
-- initially created.
updateDimensionResponse_creationDate :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.UTCTime)
updateDimensionResponse_creationDate = Lens.lens (\UpdateDimensionResponse' {creationDate} -> creationDate) (\s@UpdateDimensionResponse' {} a -> s {creationDate = a} :: UpdateDimensionResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time, in milliseconds since epoch, when the dimension was
-- most recently updated.
updateDimensionResponse_lastModifiedDate :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.UTCTime)
updateDimensionResponse_lastModifiedDate = Lens.lens (\UpdateDimensionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateDimensionResponse' {} a -> s {lastModifiedDate = a} :: UpdateDimensionResponse) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the dimension.
updateDimensionResponse_name :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.Text)
updateDimensionResponse_name = Lens.lens (\UpdateDimensionResponse' {name} -> name) (\s@UpdateDimensionResponse' {} a -> s {name = a} :: UpdateDimensionResponse)

-- | The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
updateDimensionResponse_stringValues :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateDimensionResponse_stringValues = Lens.lens (\UpdateDimensionResponse' {stringValues} -> stringValues) (\s@UpdateDimensionResponse' {} a -> s {stringValues = a} :: UpdateDimensionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of the dimension.
updateDimensionResponse_type :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe DimensionType)
updateDimensionResponse_type = Lens.lens (\UpdateDimensionResponse' {type'} -> type') (\s@UpdateDimensionResponse' {} a -> s {type' = a} :: UpdateDimensionResponse)

-- | The response's http status code.
updateDimensionResponse_httpStatus :: Lens.Lens' UpdateDimensionResponse Prelude.Int
updateDimensionResponse_httpStatus = Lens.lens (\UpdateDimensionResponse' {httpStatus} -> httpStatus) (\s@UpdateDimensionResponse' {} a -> s {httpStatus = a} :: UpdateDimensionResponse)

instance Prelude.NFData UpdateDimensionResponse where
  rnf UpdateDimensionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stringValues
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
