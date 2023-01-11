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
-- Module      : Amazonka.Personalize.DescribeSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a schema. For more information on schemas, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSchema.html CreateSchema>.
module Amazonka.Personalize.DescribeSchema
  ( -- * Creating a Request
    DescribeSchema (..),
    newDescribeSchema,

    -- * Request Lenses
    describeSchema_schemaArn,

    -- * Destructuring the Response
    DescribeSchemaResponse (..),
    newDescribeSchemaResponse,

    -- * Response Lenses
    describeSchemaResponse_schema,
    describeSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSchema' smart constructor.
data DescribeSchema = DescribeSchema'
  { -- | The Amazon Resource Name (ARN) of the schema to retrieve.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'describeSchema_schemaArn' - The Amazon Resource Name (ARN) of the schema to retrieve.
newDescribeSchema ::
  -- | 'schemaArn'
  Prelude.Text ->
  DescribeSchema
newDescribeSchema pSchemaArn_ =
  DescribeSchema' {schemaArn = pSchemaArn_}

-- | The Amazon Resource Name (ARN) of the schema to retrieve.
describeSchema_schemaArn :: Lens.Lens' DescribeSchema Prelude.Text
describeSchema_schemaArn = Lens.lens (\DescribeSchema' {schemaArn} -> schemaArn) (\s@DescribeSchema' {} a -> s {schemaArn = a} :: DescribeSchema)

instance Core.AWSRequest DescribeSchema where
  type
    AWSResponse DescribeSchema =
      DescribeSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSchemaResponse'
            Prelude.<$> (x Data..?> "schema")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSchema where
  hashWithSalt _salt DescribeSchema' {..} =
    _salt `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData DescribeSchema where
  rnf DescribeSchema' {..} = Prelude.rnf schemaArn

instance Data.ToHeaders DescribeSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeSchema" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSchema where
  toJSON DescribeSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("schemaArn" Data..= schemaArn)]
      )

instance Data.ToPath DescribeSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSchemaResponse' smart constructor.
data DescribeSchemaResponse = DescribeSchemaResponse'
  { -- | The requested schema.
    schema :: Prelude.Maybe DatasetSchema,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schema', 'describeSchemaResponse_schema' - The requested schema.
--
-- 'httpStatus', 'describeSchemaResponse_httpStatus' - The response's http status code.
newDescribeSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSchemaResponse
newDescribeSchemaResponse pHttpStatus_ =
  DescribeSchemaResponse'
    { schema = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested schema.
describeSchemaResponse_schema :: Lens.Lens' DescribeSchemaResponse (Prelude.Maybe DatasetSchema)
describeSchemaResponse_schema = Lens.lens (\DescribeSchemaResponse' {schema} -> schema) (\s@DescribeSchemaResponse' {} a -> s {schema = a} :: DescribeSchemaResponse)

-- | The response's http status code.
describeSchemaResponse_httpStatus :: Lens.Lens' DescribeSchemaResponse Prelude.Int
describeSchemaResponse_httpStatus = Lens.lens (\DescribeSchemaResponse' {httpStatus} -> httpStatus) (\s@DescribeSchemaResponse' {} a -> s {httpStatus = a} :: DescribeSchemaResponse)

instance Prelude.NFData DescribeSchemaResponse where
  rnf DescribeSchemaResponse' {..} =
    Prelude.rnf schema
      `Prelude.seq` Prelude.rnf httpStatus
