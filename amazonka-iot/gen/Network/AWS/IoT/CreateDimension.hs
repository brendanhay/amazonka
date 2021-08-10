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
-- Module      : Network.AWS.IoT.CreateDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a dimension that you can use to limit the scope of a metric used
-- in a security profile for AWS IoT Device Defender. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric
-- only to MQTT topics whose name match the pattern specified in the
-- dimension.
module Network.AWS.IoT.CreateDimension
  ( -- * Creating a Request
    CreateDimension (..),
    newCreateDimension,

    -- * Request Lenses
    createDimension_tags,
    createDimension_name,
    createDimension_type,
    createDimension_stringValues,
    createDimension_clientRequestToken,

    -- * Destructuring the Response
    CreateDimensionResponse (..),
    newCreateDimensionResponse,

    -- * Response Lenses
    createDimensionResponse_arn,
    createDimensionResponse_name,
    createDimensionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDimension' smart constructor.
data CreateDimension = CreateDimension'
  { -- | Metadata that can be used to manage the dimension.
    tags :: Prelude.Maybe [Tag],
    -- | A unique identifier for the dimension. Choose something that describes
    -- the type and value to make it easy to remember what it does.
    name :: Prelude.Text,
    -- | Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
    type' :: DimensionType,
    -- | Specifies the value or list of values for the dimension. For
    -- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
    -- topic (for example, \"admin\/#\").
    stringValues :: Prelude.NonEmpty Prelude.Text,
    -- | Each dimension must have a unique client request token. If you try to
    -- create a new dimension with the same token as a dimension that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDimension_tags' - Metadata that can be used to manage the dimension.
--
-- 'name', 'createDimension_name' - A unique identifier for the dimension. Choose something that describes
-- the type and value to make it easy to remember what it does.
--
-- 'type'', 'createDimension_type' - Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
--
-- 'stringValues', 'createDimension_stringValues' - Specifies the value or list of values for the dimension. For
-- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
-- topic (for example, \"admin\/#\").
--
-- 'clientRequestToken', 'createDimension_clientRequestToken' - Each dimension must have a unique client request token. If you try to
-- create a new dimension with the same token as a dimension that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
newCreateDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DimensionType ->
  -- | 'stringValues'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateDimension
newCreateDimension
  pName_
  pType_
  pStringValues_
  pClientRequestToken_ =
    CreateDimension'
      { tags = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        stringValues = Lens._Coerce Lens.# pStringValues_,
        clientRequestToken = pClientRequestToken_
      }

-- | Metadata that can be used to manage the dimension.
createDimension_tags :: Lens.Lens' CreateDimension (Prelude.Maybe [Tag])
createDimension_tags = Lens.lens (\CreateDimension' {tags} -> tags) (\s@CreateDimension' {} a -> s {tags = a} :: CreateDimension) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for the dimension. Choose something that describes
-- the type and value to make it easy to remember what it does.
createDimension_name :: Lens.Lens' CreateDimension Prelude.Text
createDimension_name = Lens.lens (\CreateDimension' {name} -> name) (\s@CreateDimension' {} a -> s {name = a} :: CreateDimension)

-- | Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
createDimension_type :: Lens.Lens' CreateDimension DimensionType
createDimension_type = Lens.lens (\CreateDimension' {type'} -> type') (\s@CreateDimension' {} a -> s {type' = a} :: CreateDimension)

-- | Specifies the value or list of values for the dimension. For
-- @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT
-- topic (for example, \"admin\/#\").
createDimension_stringValues :: Lens.Lens' CreateDimension (Prelude.NonEmpty Prelude.Text)
createDimension_stringValues = Lens.lens (\CreateDimension' {stringValues} -> stringValues) (\s@CreateDimension' {} a -> s {stringValues = a} :: CreateDimension) Prelude.. Lens._Coerce

-- | Each dimension must have a unique client request token. If you try to
-- create a new dimension with the same token as a dimension that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createDimension_clientRequestToken :: Lens.Lens' CreateDimension Prelude.Text
createDimension_clientRequestToken = Lens.lens (\CreateDimension' {clientRequestToken} -> clientRequestToken) (\s@CreateDimension' {} a -> s {clientRequestToken = a} :: CreateDimension)

instance Core.AWSRequest CreateDimension where
  type
    AWSResponse CreateDimension =
      CreateDimensionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDimensionResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDimension

instance Prelude.NFData CreateDimension

instance Core.ToHeaders CreateDimension where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateDimension where
  toJSON CreateDimension' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("stringValues" Core..= stringValues),
            Prelude.Just
              ("clientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath CreateDimension where
  toPath CreateDimension' {..} =
    Prelude.mconcat ["/dimensions/", Core.toBS name]

instance Core.ToQuery CreateDimension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDimensionResponse' smart constructor.
data CreateDimensionResponse = CreateDimensionResponse'
  { -- | The Amazon Resource Name (ARN) of the created dimension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the dimension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDimensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createDimensionResponse_arn' - The Amazon Resource Name (ARN) of the created dimension.
--
-- 'name', 'createDimensionResponse_name' - A unique identifier for the dimension.
--
-- 'httpStatus', 'createDimensionResponse_httpStatus' - The response's http status code.
newCreateDimensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDimensionResponse
newCreateDimensionResponse pHttpStatus_ =
  CreateDimensionResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created dimension.
createDimensionResponse_arn :: Lens.Lens' CreateDimensionResponse (Prelude.Maybe Prelude.Text)
createDimensionResponse_arn = Lens.lens (\CreateDimensionResponse' {arn} -> arn) (\s@CreateDimensionResponse' {} a -> s {arn = a} :: CreateDimensionResponse)

-- | A unique identifier for the dimension.
createDimensionResponse_name :: Lens.Lens' CreateDimensionResponse (Prelude.Maybe Prelude.Text)
createDimensionResponse_name = Lens.lens (\CreateDimensionResponse' {name} -> name) (\s@CreateDimensionResponse' {} a -> s {name = a} :: CreateDimensionResponse)

-- | The response's http status code.
createDimensionResponse_httpStatus :: Lens.Lens' CreateDimensionResponse Prelude.Int
createDimensionResponse_httpStatus = Lens.lens (\CreateDimensionResponse' {httpStatus} -> httpStatus) (\s@CreateDimensionResponse' {} a -> s {httpStatus = a} :: CreateDimensionResponse)

instance Prelude.NFData CreateDimensionResponse
