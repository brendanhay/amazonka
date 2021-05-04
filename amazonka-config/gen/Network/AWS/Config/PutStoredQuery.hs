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
-- Module      : Network.AWS.Config.PutStoredQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Saves a new query or updates an existing saved query. The @QueryName@
-- must be unique for a single AWS account and a single AWS Region. You can
-- create upto 300 queries in a single AWS account and a single AWS Region.
module Network.AWS.Config.PutStoredQuery
  ( -- * Creating a Request
    PutStoredQuery (..),
    newPutStoredQuery,

    -- * Request Lenses
    putStoredQuery_tags,
    putStoredQuery_storedQuery,

    -- * Destructuring the Response
    PutStoredQueryResponse (..),
    newPutStoredQueryResponse,

    -- * Response Lenses
    putStoredQueryResponse_queryArn,
    putStoredQueryResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutStoredQuery' smart constructor.
data PutStoredQuery = PutStoredQuery'
  { -- | A list of @Tags@ object.
    tags :: Prelude.Maybe [Tag],
    -- | A list of @StoredQuery@ objects. The mandatory fields are @QueryName@
    -- and @Expression@.
    --
    -- When you are creating a query, you must provide a query name and an
    -- expression. When you are updating a query, you must provide a query name
    -- but updating the description is optional.
    storedQuery :: StoredQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutStoredQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putStoredQuery_tags' - A list of @Tags@ object.
--
-- 'storedQuery', 'putStoredQuery_storedQuery' - A list of @StoredQuery@ objects. The mandatory fields are @QueryName@
-- and @Expression@.
--
-- When you are creating a query, you must provide a query name and an
-- expression. When you are updating a query, you must provide a query name
-- but updating the description is optional.
newPutStoredQuery ::
  -- | 'storedQuery'
  StoredQuery ->
  PutStoredQuery
newPutStoredQuery pStoredQuery_ =
  PutStoredQuery'
    { tags = Prelude.Nothing,
      storedQuery = pStoredQuery_
    }

-- | A list of @Tags@ object.
putStoredQuery_tags :: Lens.Lens' PutStoredQuery (Prelude.Maybe [Tag])
putStoredQuery_tags = Lens.lens (\PutStoredQuery' {tags} -> tags) (\s@PutStoredQuery' {} a -> s {tags = a} :: PutStoredQuery) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of @StoredQuery@ objects. The mandatory fields are @QueryName@
-- and @Expression@.
--
-- When you are creating a query, you must provide a query name and an
-- expression. When you are updating a query, you must provide a query name
-- but updating the description is optional.
putStoredQuery_storedQuery :: Lens.Lens' PutStoredQuery StoredQuery
putStoredQuery_storedQuery = Lens.lens (\PutStoredQuery' {storedQuery} -> storedQuery) (\s@PutStoredQuery' {} a -> s {storedQuery = a} :: PutStoredQuery)

instance Prelude.AWSRequest PutStoredQuery where
  type Rs PutStoredQuery = PutStoredQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutStoredQueryResponse'
            Prelude.<$> (x Prelude..?> "QueryArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutStoredQuery

instance Prelude.NFData PutStoredQuery

instance Prelude.ToHeaders PutStoredQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.PutStoredQuery" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutStoredQuery where
  toJSON PutStoredQuery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("StoredQuery" Prelude..= storedQuery)
          ]
      )

instance Prelude.ToPath PutStoredQuery where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutStoredQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutStoredQueryResponse' smart constructor.
data PutStoredQueryResponse = PutStoredQueryResponse'
  { -- | Amazon Resource Name (ARN) of the query. For example,
    -- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
    queryArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutStoredQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryArn', 'putStoredQueryResponse_queryArn' - Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
--
-- 'httpStatus', 'putStoredQueryResponse_httpStatus' - The response's http status code.
newPutStoredQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutStoredQueryResponse
newPutStoredQueryResponse pHttpStatus_ =
  PutStoredQueryResponse'
    { queryArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
putStoredQueryResponse_queryArn :: Lens.Lens' PutStoredQueryResponse (Prelude.Maybe Prelude.Text)
putStoredQueryResponse_queryArn = Lens.lens (\PutStoredQueryResponse' {queryArn} -> queryArn) (\s@PutStoredQueryResponse' {} a -> s {queryArn = a} :: PutStoredQueryResponse)

-- | The response's http status code.
putStoredQueryResponse_httpStatus :: Lens.Lens' PutStoredQueryResponse Prelude.Int
putStoredQueryResponse_httpStatus = Lens.lens (\PutStoredQueryResponse' {httpStatus} -> httpStatus) (\s@PutStoredQueryResponse' {} a -> s {httpStatus = a} :: PutStoredQueryResponse)

instance Prelude.NFData PutStoredQueryResponse
