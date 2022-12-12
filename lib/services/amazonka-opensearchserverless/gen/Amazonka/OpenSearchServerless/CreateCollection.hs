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
-- Module      : Amazonka.OpenSearchServerless.CreateCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OpenSearch Serverless collection. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-manage.html Creating and managing Amazon OpenSearch Serverless collections>.
module Amazonka.OpenSearchServerless.CreateCollection
  ( -- * Creating a Request
    CreateCollection (..),
    newCreateCollection,

    -- * Request Lenses
    createCollection_clientToken,
    createCollection_description,
    createCollection_tags,
    createCollection_type,
    createCollection_name,

    -- * Destructuring the Response
    CreateCollectionResponse (..),
    newCreateCollectionResponse,

    -- * Response Lenses
    createCollectionResponse_createCollectionDetail,
    createCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCollection' smart constructor.
data CreateCollection = CreateCollection'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Description of the collection.
    description :: Prelude.Maybe Prelude.Text,
    -- | An arbitrary set of tags (key–value pairs) to associate with the
    -- OpenSearch Serverless collection.
    tags :: Prelude.Maybe [Tag],
    -- | The type of collection.
    type' :: Prelude.Maybe CollectionType,
    -- | Name of the collection.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createCollection_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'createCollection_description' - Description of the collection.
--
-- 'tags', 'createCollection_tags' - An arbitrary set of tags (key–value pairs) to associate with the
-- OpenSearch Serverless collection.
--
-- 'type'', 'createCollection_type' - The type of collection.
--
-- 'name', 'createCollection_name' - Name of the collection.
newCreateCollection ::
  -- | 'name'
  Prelude.Text ->
  CreateCollection
newCreateCollection pName_ =
  CreateCollection'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
createCollection_clientToken :: Lens.Lens' CreateCollection (Prelude.Maybe Prelude.Text)
createCollection_clientToken = Lens.lens (\CreateCollection' {clientToken} -> clientToken) (\s@CreateCollection' {} a -> s {clientToken = a} :: CreateCollection)

-- | Description of the collection.
createCollection_description :: Lens.Lens' CreateCollection (Prelude.Maybe Prelude.Text)
createCollection_description = Lens.lens (\CreateCollection' {description} -> description) (\s@CreateCollection' {} a -> s {description = a} :: CreateCollection)

-- | An arbitrary set of tags (key–value pairs) to associate with the
-- OpenSearch Serverless collection.
createCollection_tags :: Lens.Lens' CreateCollection (Prelude.Maybe [Tag])
createCollection_tags = Lens.lens (\CreateCollection' {tags} -> tags) (\s@CreateCollection' {} a -> s {tags = a} :: CreateCollection) Prelude.. Lens.mapping Lens.coerced

-- | The type of collection.
createCollection_type :: Lens.Lens' CreateCollection (Prelude.Maybe CollectionType)
createCollection_type = Lens.lens (\CreateCollection' {type'} -> type') (\s@CreateCollection' {} a -> s {type' = a} :: CreateCollection)

-- | Name of the collection.
createCollection_name :: Lens.Lens' CreateCollection Prelude.Text
createCollection_name = Lens.lens (\CreateCollection' {name} -> name) (\s@CreateCollection' {} a -> s {name = a} :: CreateCollection)

instance Core.AWSRequest CreateCollection where
  type
    AWSResponse CreateCollection =
      CreateCollectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCollectionResponse'
            Prelude.<$> (x Data..?> "createCollectionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCollection where
  hashWithSalt _salt CreateCollection' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateCollection where
  rnf CreateCollection' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.CreateCollection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCollection where
  toJSON CreateCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCollectionResponse' smart constructor.
data CreateCollectionResponse = CreateCollectionResponse'
  { -- | Details about the collection.
    createCollectionDetail :: Prelude.Maybe CreateCollectionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createCollectionDetail', 'createCollectionResponse_createCollectionDetail' - Details about the collection.
--
-- 'httpStatus', 'createCollectionResponse_httpStatus' - The response's http status code.
newCreateCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCollectionResponse
newCreateCollectionResponse pHttpStatus_ =
  CreateCollectionResponse'
    { createCollectionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the collection.
createCollectionResponse_createCollectionDetail :: Lens.Lens' CreateCollectionResponse (Prelude.Maybe CreateCollectionDetail)
createCollectionResponse_createCollectionDetail = Lens.lens (\CreateCollectionResponse' {createCollectionDetail} -> createCollectionDetail) (\s@CreateCollectionResponse' {} a -> s {createCollectionDetail = a} :: CreateCollectionResponse)

-- | The response's http status code.
createCollectionResponse_httpStatus :: Lens.Lens' CreateCollectionResponse Prelude.Int
createCollectionResponse_httpStatus = Lens.lens (\CreateCollectionResponse' {httpStatus} -> httpStatus) (\s@CreateCollectionResponse' {} a -> s {httpStatus = a} :: CreateCollectionResponse)

instance Prelude.NFData CreateCollectionResponse where
  rnf CreateCollectionResponse' {..} =
    Prelude.rnf createCollectionDetail
      `Prelude.seq` Prelude.rnf httpStatus
