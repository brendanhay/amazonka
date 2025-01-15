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
-- Module      : Amazonka.Personalize.CreateSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Personalize schema from the specified schema string.
-- The schema you create must be in Avro JSON format.
--
-- Amazon Personalize recognizes three schema variants. Each schema is
-- associated with a dataset type and has a set of required field and
-- keywords. If you are creating a schema for a dataset in a Domain dataset
-- group, you provide the domain of the Domain dataset group. You specify a
-- schema when you call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDataset.html CreateDataset>.
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListSchemas.html ListSchemas>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSchema.html DescribeSchema>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DeleteSchema.html DeleteSchema>
module Amazonka.Personalize.CreateSchema
  ( -- * Creating a Request
    CreateSchema (..),
    newCreateSchema,

    -- * Request Lenses
    createSchema_domain,
    createSchema_name,
    createSchema_schema,

    -- * Destructuring the Response
    CreateSchemaResponse (..),
    newCreateSchemaResponse,

    -- * Response Lenses
    createSchemaResponse_schemaArn,
    createSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { -- | The domain for the schema. If you are creating a schema for a dataset in
    -- a Domain dataset group, specify the domain you chose when you created
    -- the Domain dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The name for the schema.
    name :: Prelude.Text,
    -- | A schema in Avro JSON format.
    schema :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createSchema_domain' - The domain for the schema. If you are creating a schema for a dataset in
-- a Domain dataset group, specify the domain you chose when you created
-- the Domain dataset group.
--
-- 'name', 'createSchema_name' - The name for the schema.
--
-- 'schema', 'createSchema_schema' - A schema in Avro JSON format.
newCreateSchema ::
  -- | 'name'
  Prelude.Text ->
  -- | 'schema'
  Prelude.Text ->
  CreateSchema
newCreateSchema pName_ pSchema_ =
  CreateSchema'
    { domain = Prelude.Nothing,
      name = pName_,
      schema = pSchema_
    }

-- | The domain for the schema. If you are creating a schema for a dataset in
-- a Domain dataset group, specify the domain you chose when you created
-- the Domain dataset group.
createSchema_domain :: Lens.Lens' CreateSchema (Prelude.Maybe Domain)
createSchema_domain = Lens.lens (\CreateSchema' {domain} -> domain) (\s@CreateSchema' {} a -> s {domain = a} :: CreateSchema)

-- | The name for the schema.
createSchema_name :: Lens.Lens' CreateSchema Prelude.Text
createSchema_name = Lens.lens (\CreateSchema' {name} -> name) (\s@CreateSchema' {} a -> s {name = a} :: CreateSchema)

-- | A schema in Avro JSON format.
createSchema_schema :: Lens.Lens' CreateSchema Prelude.Text
createSchema_schema = Lens.lens (\CreateSchema' {schema} -> schema) (\s@CreateSchema' {} a -> s {schema = a} :: CreateSchema)

instance Core.AWSRequest CreateSchema where
  type AWSResponse CreateSchema = CreateSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Prelude.<$> (x Data..?> "schemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSchema where
  hashWithSalt _salt CreateSchema' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schema

instance Prelude.NFData CreateSchema where
  rnf CreateSchema' {..} =
    Prelude.rnf domain `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf schema

instance Data.ToHeaders CreateSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateSchema" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domain" Data..=) Prelude.<$> domain,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("schema" Data..= schema)
          ]
      )

instance Data.ToPath CreateSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The Amazon Resource Name (ARN) of the created schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'createSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the created schema.
--
-- 'httpStatus', 'createSchemaResponse_httpStatus' - The response's http status code.
newCreateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSchemaResponse
newCreateSchemaResponse pHttpStatus_ =
  CreateSchemaResponse'
    { schemaArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created schema.
createSchemaResponse_schemaArn :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaArn = Lens.lens (\CreateSchemaResponse' {schemaArn} -> schemaArn) (\s@CreateSchemaResponse' {} a -> s {schemaArn = a} :: CreateSchemaResponse)

-- | The response's http status code.
createSchemaResponse_httpStatus :: Lens.Lens' CreateSchemaResponse Prelude.Int
createSchemaResponse_httpStatus = Lens.lens (\CreateSchemaResponse' {httpStatus} -> httpStatus) (\s@CreateSchemaResponse' {} a -> s {httpStatus = a} :: CreateSchemaResponse)

instance Prelude.NFData CreateSchemaResponse where
  rnf CreateSchemaResponse' {..} =
    Prelude.rnf schemaArn `Prelude.seq`
      Prelude.rnf httpStatus
