{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.UpdateDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for a dimension. You cannot change the type of a
-- dimension after it is created (you can delete it and recreate it).
module Network.AWS.IoT.UpdateDimension
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
    updateDimensionResponse_lastModifiedDate,
    updateDimensionResponse_creationDate,
    updateDimensionResponse_arn,
    updateDimensionResponse_name,
    updateDimensionResponse_type,
    updateDimensionResponse_stringValues,
    updateDimensionResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      stringValues = Prelude._Coerce Lens.# pStringValues_
    }

-- | A unique identifier for the dimension. Choose something that describes
-- the type and value to make it easy to remember what it does.
updateDimension_name :: Lens.Lens' UpdateDimension Prelude.Text
updateDimension_name = Lens.lens (\UpdateDimension' {name} -> name) (\s@UpdateDimension' {} a -> s {name = a} :: UpdateDimension)

-- | Specifies the value or list of values for the dimension. For
-- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
-- topic (for example, \"admin\/#\").
updateDimension_stringValues :: Lens.Lens' UpdateDimension (Prelude.NonEmpty Prelude.Text)
updateDimension_stringValues = Lens.lens (\UpdateDimension' {stringValues} -> stringValues) (\s@UpdateDimension' {} a -> s {stringValues = a} :: UpdateDimension) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UpdateDimension where
  type Rs UpdateDimension = UpdateDimensionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDimensionResponse'
            Prelude.<$> (x Prelude..?> "lastModifiedDate")
            Prelude.<*> (x Prelude..?> "creationDate")
            Prelude.<*> (x Prelude..?> "arn")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "type")
            Prelude.<*> (x Prelude..?> "stringValues")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDimension

instance Prelude.NFData UpdateDimension

instance Prelude.ToHeaders UpdateDimension where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateDimension where
  toJSON UpdateDimension' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("stringValues" Prelude..= stringValues)
          ]
      )

instance Prelude.ToPath UpdateDimension where
  toPath UpdateDimension' {..} =
    Prelude.mconcat ["/dimensions/", Prelude.toBS name]

instance Prelude.ToQuery UpdateDimension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDimensionResponse' smart constructor.
data UpdateDimensionResponse = UpdateDimensionResponse'
  { -- | The date and time, in milliseconds since epoch, when the dimension was
    -- most recently updated.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time, in milliseconds since epoch, when the dimension was
    -- initially created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN)of the created dimension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the dimension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the dimension.
    type' :: Prelude.Maybe DimensionType,
    -- | The value or list of values used to scope the dimension. For example,
    -- for topic filters, this is the pattern used to match the MQTT topic
    -- name.
    stringValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDimensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'updateDimensionResponse_lastModifiedDate' - The date and time, in milliseconds since epoch, when the dimension was
-- most recently updated.
--
-- 'creationDate', 'updateDimensionResponse_creationDate' - The date and time, in milliseconds since epoch, when the dimension was
-- initially created.
--
-- 'arn', 'updateDimensionResponse_arn' - The Amazon Resource Name (ARN)of the created dimension.
--
-- 'name', 'updateDimensionResponse_name' - A unique identifier for the dimension.
--
-- 'type'', 'updateDimensionResponse_type' - The type of the dimension.
--
-- 'stringValues', 'updateDimensionResponse_stringValues' - The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
--
-- 'httpStatus', 'updateDimensionResponse_httpStatus' - The response's http status code.
newUpdateDimensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDimensionResponse
newUpdateDimensionResponse pHttpStatus_ =
  UpdateDimensionResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      stringValues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time, in milliseconds since epoch, when the dimension was
-- most recently updated.
updateDimensionResponse_lastModifiedDate :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.UTCTime)
updateDimensionResponse_lastModifiedDate = Lens.lens (\UpdateDimensionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateDimensionResponse' {} a -> s {lastModifiedDate = a} :: UpdateDimensionResponse) Prelude.. Lens.mapping Prelude._Time

-- | The date and time, in milliseconds since epoch, when the dimension was
-- initially created.
updateDimensionResponse_creationDate :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.UTCTime)
updateDimensionResponse_creationDate = Lens.lens (\UpdateDimensionResponse' {creationDate} -> creationDate) (\s@UpdateDimensionResponse' {} a -> s {creationDate = a} :: UpdateDimensionResponse) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN)of the created dimension.
updateDimensionResponse_arn :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.Text)
updateDimensionResponse_arn = Lens.lens (\UpdateDimensionResponse' {arn} -> arn) (\s@UpdateDimensionResponse' {} a -> s {arn = a} :: UpdateDimensionResponse)

-- | A unique identifier for the dimension.
updateDimensionResponse_name :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe Prelude.Text)
updateDimensionResponse_name = Lens.lens (\UpdateDimensionResponse' {name} -> name) (\s@UpdateDimensionResponse' {} a -> s {name = a} :: UpdateDimensionResponse)

-- | The type of the dimension.
updateDimensionResponse_type :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe DimensionType)
updateDimensionResponse_type = Lens.lens (\UpdateDimensionResponse' {type'} -> type') (\s@UpdateDimensionResponse' {} a -> s {type' = a} :: UpdateDimensionResponse)

-- | The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
updateDimensionResponse_stringValues :: Lens.Lens' UpdateDimensionResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateDimensionResponse_stringValues = Lens.lens (\UpdateDimensionResponse' {stringValues} -> stringValues) (\s@UpdateDimensionResponse' {} a -> s {stringValues = a} :: UpdateDimensionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
updateDimensionResponse_httpStatus :: Lens.Lens' UpdateDimensionResponse Prelude.Int
updateDimensionResponse_httpStatus = Lens.lens (\UpdateDimensionResponse' {httpStatus} -> httpStatus) (\s@UpdateDimensionResponse' {} a -> s {httpStatus = a} :: UpdateDimensionResponse)

instance Prelude.NFData UpdateDimensionResponse
