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
-- Module      : Network.AWS.CloudDirectory.CreateSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema in a development state. A schema can exist in three
-- phases:
--
-- -   /Development:/ This is a mutable phase of the schema. All new
--     schemas are in the development phase. Once the schema is finalized,
--     it can be published.
--
-- -   /Published:/ Published schemas are immutable and have a version
--     associated with them.
--
-- -   /Applied:/ Applied schemas are mutable in a way that allows you to
--     add new schema facets. You can also add new, nonrequired attributes
--     to existing schema facets. You can apply only published schemas to
--     directories.
module Network.AWS.CloudDirectory.CreateSchema
  ( -- * Creating a Request
    CreateSchema (..),
    newCreateSchema,

    -- * Request Lenses
    createSchema_name,

    -- * Destructuring the Response
    CreateSchemaResponse (..),
    newCreateSchemaResponse,

    -- * Response Lenses
    createSchemaResponse_schemaArn,
    createSchemaResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { -- | The name that is associated with the schema. This is unique to each
    -- account and in each region.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSchema_name' - The name that is associated with the schema. This is unique to each
-- account and in each region.
newCreateSchema ::
  -- | 'name'
  Prelude.Text ->
  CreateSchema
newCreateSchema pName_ = CreateSchema' {name = pName_}

-- | The name that is associated with the schema. This is unique to each
-- account and in each region.
createSchema_name :: Lens.Lens' CreateSchema Prelude.Text
createSchema_name = Lens.lens (\CreateSchema' {name} -> name) (\s@CreateSchema' {} a -> s {name = a} :: CreateSchema)

instance Prelude.AWSRequest CreateSchema where
  type Rs CreateSchema = CreateSchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Prelude.<$> (x Prelude..?> "SchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSchema

instance Prelude.NFData CreateSchema

instance Prelude.ToHeaders CreateSchema where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath CreateSchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/create"

instance Prelude.ToQuery CreateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'createSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
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

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
createSchemaResponse_schemaArn :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaArn = Lens.lens (\CreateSchemaResponse' {schemaArn} -> schemaArn) (\s@CreateSchemaResponse' {} a -> s {schemaArn = a} :: CreateSchemaResponse)

-- | The response's http status code.
createSchemaResponse_httpStatus :: Lens.Lens' CreateSchemaResponse Prelude.Int
createSchemaResponse_httpStatus = Lens.lens (\CreateSchemaResponse' {httpStatus} -> httpStatus) (\s@CreateSchemaResponse' {} a -> s {httpStatus = a} :: CreateSchemaResponse)

instance Prelude.NFData CreateSchemaResponse
