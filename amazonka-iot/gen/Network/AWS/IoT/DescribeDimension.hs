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
-- Module      : Network.AWS.IoT.DescribeDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a dimension that is defined in your AWS account.
module Network.AWS.IoT.DescribeDimension
  ( -- * Creating a Request
    DescribeDimension (..),
    newDescribeDimension,

    -- * Request Lenses
    describeDimension_name,

    -- * Destructuring the Response
    DescribeDimensionResponse (..),
    newDescribeDimensionResponse,

    -- * Response Lenses
    describeDimensionResponse_lastModifiedDate,
    describeDimensionResponse_creationDate,
    describeDimensionResponse_arn,
    describeDimensionResponse_name,
    describeDimensionResponse_type,
    describeDimensionResponse_stringValues,
    describeDimensionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDimension' smart constructor.
data DescribeDimension = DescribeDimension'
  { -- | The unique identifier for the dimension.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeDimension_name' - The unique identifier for the dimension.
newDescribeDimension ::
  -- | 'name'
  Core.Text ->
  DescribeDimension
newDescribeDimension pName_ =
  DescribeDimension' {name = pName_}

-- | The unique identifier for the dimension.
describeDimension_name :: Lens.Lens' DescribeDimension Core.Text
describeDimension_name = Lens.lens (\DescribeDimension' {name} -> name) (\s@DescribeDimension' {} a -> s {name = a} :: DescribeDimension)

instance Core.AWSRequest DescribeDimension where
  type
    AWSResponse DescribeDimension =
      DescribeDimensionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDimensionResponse'
            Core.<$> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "type")
            Core.<*> (x Core..?> "stringValues")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDimension

instance Core.NFData DescribeDimension

instance Core.ToHeaders DescribeDimension where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDimension where
  toPath DescribeDimension' {..} =
    Core.mconcat ["/dimensions/", Core.toBS name]

instance Core.ToQuery DescribeDimension where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDimensionResponse' smart constructor.
data DescribeDimensionResponse = DescribeDimensionResponse'
  { -- | The date the dimension was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The date the dimension was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) for the dimension.
    arn :: Core.Maybe Core.Text,
    -- | The unique identifier for the dimension.
    name :: Core.Maybe Core.Text,
    -- | The type of the dimension.
    type' :: Core.Maybe DimensionType,
    -- | The value or list of values used to scope the dimension. For example,
    -- for topic filters, this is the pattern used to match the MQTT topic
    -- name.
    stringValues :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDimensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'describeDimensionResponse_lastModifiedDate' - The date the dimension was last modified.
--
-- 'creationDate', 'describeDimensionResponse_creationDate' - The date the dimension was created.
--
-- 'arn', 'describeDimensionResponse_arn' - The Amazon Resource Name (ARN) for the dimension.
--
-- 'name', 'describeDimensionResponse_name' - The unique identifier for the dimension.
--
-- 'type'', 'describeDimensionResponse_type' - The type of the dimension.
--
-- 'stringValues', 'describeDimensionResponse_stringValues' - The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
--
-- 'httpStatus', 'describeDimensionResponse_httpStatus' - The response's http status code.
newDescribeDimensionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDimensionResponse
newDescribeDimensionResponse pHttpStatus_ =
  DescribeDimensionResponse'
    { lastModifiedDate =
        Core.Nothing,
      creationDate = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing,
      stringValues = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the dimension was last modified.
describeDimensionResponse_lastModifiedDate :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Core.UTCTime)
describeDimensionResponse_lastModifiedDate = Lens.lens (\DescribeDimensionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeDimensionResponse' {} a -> s {lastModifiedDate = a} :: DescribeDimensionResponse) Core.. Lens.mapping Core._Time

-- | The date the dimension was created.
describeDimensionResponse_creationDate :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Core.UTCTime)
describeDimensionResponse_creationDate = Lens.lens (\DescribeDimensionResponse' {creationDate} -> creationDate) (\s@DescribeDimensionResponse' {} a -> s {creationDate = a} :: DescribeDimensionResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) for the dimension.
describeDimensionResponse_arn :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Core.Text)
describeDimensionResponse_arn = Lens.lens (\DescribeDimensionResponse' {arn} -> arn) (\s@DescribeDimensionResponse' {} a -> s {arn = a} :: DescribeDimensionResponse)

-- | The unique identifier for the dimension.
describeDimensionResponse_name :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Core.Text)
describeDimensionResponse_name = Lens.lens (\DescribeDimensionResponse' {name} -> name) (\s@DescribeDimensionResponse' {} a -> s {name = a} :: DescribeDimensionResponse)

-- | The type of the dimension.
describeDimensionResponse_type :: Lens.Lens' DescribeDimensionResponse (Core.Maybe DimensionType)
describeDimensionResponse_type = Lens.lens (\DescribeDimensionResponse' {type'} -> type') (\s@DescribeDimensionResponse' {} a -> s {type' = a} :: DescribeDimensionResponse)

-- | The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
describeDimensionResponse_stringValues :: Lens.Lens' DescribeDimensionResponse (Core.Maybe (Core.NonEmpty Core.Text))
describeDimensionResponse_stringValues = Lens.lens (\DescribeDimensionResponse' {stringValues} -> stringValues) (\s@DescribeDimensionResponse' {} a -> s {stringValues = a} :: DescribeDimensionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDimensionResponse_httpStatus :: Lens.Lens' DescribeDimensionResponse Core.Int
describeDimensionResponse_httpStatus = Lens.lens (\DescribeDimensionResponse' {httpStatus} -> httpStatus) (\s@DescribeDimensionResponse' {} a -> s {httpStatus = a} :: DescribeDimensionResponse)

instance Core.NFData DescribeDimensionResponse
