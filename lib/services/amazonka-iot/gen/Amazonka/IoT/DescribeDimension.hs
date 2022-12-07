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
-- Module      : Amazonka.IoT.DescribeDimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a dimension that is defined in your Amazon Web
-- Services accounts.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeDimension>
-- action.
module Amazonka.IoT.DescribeDimension
  ( -- * Creating a Request
    DescribeDimension (..),
    newDescribeDimension,

    -- * Request Lenses
    describeDimension_name,

    -- * Destructuring the Response
    DescribeDimensionResponse (..),
    newDescribeDimensionResponse,

    -- * Response Lenses
    describeDimensionResponse_name,
    describeDimensionResponse_type,
    describeDimensionResponse_stringValues,
    describeDimensionResponse_lastModifiedDate,
    describeDimensionResponse_arn,
    describeDimensionResponse_creationDate,
    describeDimensionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDimension' smart constructor.
data DescribeDimension = DescribeDimension'
  { -- | The unique identifier for the dimension.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeDimension
newDescribeDimension pName_ =
  DescribeDimension' {name = pName_}

-- | The unique identifier for the dimension.
describeDimension_name :: Lens.Lens' DescribeDimension Prelude.Text
describeDimension_name = Lens.lens (\DescribeDimension' {name} -> name) (\s@DescribeDimension' {} a -> s {name = a} :: DescribeDimension)

instance Core.AWSRequest DescribeDimension where
  type
    AWSResponse DescribeDimension =
      DescribeDimensionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDimensionResponse'
            Prelude.<$> (x Data..?> "name")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (x Data..?> "stringValues")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDimension where
  hashWithSalt _salt DescribeDimension' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeDimension where
  rnf DescribeDimension' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeDimension where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDimension where
  toPath DescribeDimension' {..} =
    Prelude.mconcat ["/dimensions/", Data.toBS name]

instance Data.ToQuery DescribeDimension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDimensionResponse' smart constructor.
data DescribeDimensionResponse = DescribeDimensionResponse'
  { -- | The unique identifier for the dimension.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the dimension.
    type' :: Prelude.Maybe DimensionType,
    -- | The value or list of values used to scope the dimension. For example,
    -- for topic filters, this is the pattern used to match the MQTT topic
    -- name.
    stringValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The date the dimension was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) for the dimension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date the dimension was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDimensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeDimensionResponse_name' - The unique identifier for the dimension.
--
-- 'type'', 'describeDimensionResponse_type' - The type of the dimension.
--
-- 'stringValues', 'describeDimensionResponse_stringValues' - The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
--
-- 'lastModifiedDate', 'describeDimensionResponse_lastModifiedDate' - The date the dimension was last modified.
--
-- 'arn', 'describeDimensionResponse_arn' - The Amazon Resource Name (ARN) for the dimension.
--
-- 'creationDate', 'describeDimensionResponse_creationDate' - The date the dimension was created.
--
-- 'httpStatus', 'describeDimensionResponse_httpStatus' - The response's http status code.
newDescribeDimensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDimensionResponse
newDescribeDimensionResponse pHttpStatus_ =
  DescribeDimensionResponse'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      stringValues = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the dimension.
describeDimensionResponse_name :: Lens.Lens' DescribeDimensionResponse (Prelude.Maybe Prelude.Text)
describeDimensionResponse_name = Lens.lens (\DescribeDimensionResponse' {name} -> name) (\s@DescribeDimensionResponse' {} a -> s {name = a} :: DescribeDimensionResponse)

-- | The type of the dimension.
describeDimensionResponse_type :: Lens.Lens' DescribeDimensionResponse (Prelude.Maybe DimensionType)
describeDimensionResponse_type = Lens.lens (\DescribeDimensionResponse' {type'} -> type') (\s@DescribeDimensionResponse' {} a -> s {type' = a} :: DescribeDimensionResponse)

-- | The value or list of values used to scope the dimension. For example,
-- for topic filters, this is the pattern used to match the MQTT topic
-- name.
describeDimensionResponse_stringValues :: Lens.Lens' DescribeDimensionResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeDimensionResponse_stringValues = Lens.lens (\DescribeDimensionResponse' {stringValues} -> stringValues) (\s@DescribeDimensionResponse' {} a -> s {stringValues = a} :: DescribeDimensionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date the dimension was last modified.
describeDimensionResponse_lastModifiedDate :: Lens.Lens' DescribeDimensionResponse (Prelude.Maybe Prelude.UTCTime)
describeDimensionResponse_lastModifiedDate = Lens.lens (\DescribeDimensionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeDimensionResponse' {} a -> s {lastModifiedDate = a} :: DescribeDimensionResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the dimension.
describeDimensionResponse_arn :: Lens.Lens' DescribeDimensionResponse (Prelude.Maybe Prelude.Text)
describeDimensionResponse_arn = Lens.lens (\DescribeDimensionResponse' {arn} -> arn) (\s@DescribeDimensionResponse' {} a -> s {arn = a} :: DescribeDimensionResponse)

-- | The date the dimension was created.
describeDimensionResponse_creationDate :: Lens.Lens' DescribeDimensionResponse (Prelude.Maybe Prelude.UTCTime)
describeDimensionResponse_creationDate = Lens.lens (\DescribeDimensionResponse' {creationDate} -> creationDate) (\s@DescribeDimensionResponse' {} a -> s {creationDate = a} :: DescribeDimensionResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeDimensionResponse_httpStatus :: Lens.Lens' DescribeDimensionResponse Prelude.Int
describeDimensionResponse_httpStatus = Lens.lens (\DescribeDimensionResponse' {httpStatus} -> httpStatus) (\s@DescribeDimensionResponse' {} a -> s {httpStatus = a} :: DescribeDimensionResponse)

instance Prelude.NFData DescribeDimensionResponse where
  rnf DescribeDimensionResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf stringValues
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf httpStatus
