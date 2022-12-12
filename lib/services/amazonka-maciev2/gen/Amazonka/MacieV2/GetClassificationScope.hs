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
-- Module      : Amazonka.MacieV2.GetClassificationScope
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the classification scope settings for an account.
module Amazonka.MacieV2.GetClassificationScope
  ( -- * Creating a Request
    GetClassificationScope (..),
    newGetClassificationScope,

    -- * Request Lenses
    getClassificationScope_id,

    -- * Destructuring the Response
    GetClassificationScopeResponse (..),
    newGetClassificationScopeResponse,

    -- * Response Lenses
    getClassificationScopeResponse_id,
    getClassificationScopeResponse_name,
    getClassificationScopeResponse_s3,
    getClassificationScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClassificationScope' smart constructor.
data GetClassificationScope = GetClassificationScope'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClassificationScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getClassificationScope_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newGetClassificationScope ::
  -- | 'id'
  Prelude.Text ->
  GetClassificationScope
newGetClassificationScope pId_ =
  GetClassificationScope' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
getClassificationScope_id :: Lens.Lens' GetClassificationScope Prelude.Text
getClassificationScope_id = Lens.lens (\GetClassificationScope' {id} -> id) (\s@GetClassificationScope' {} a -> s {id = a} :: GetClassificationScope)

instance Core.AWSRequest GetClassificationScope where
  type
    AWSResponse GetClassificationScope =
      GetClassificationScopeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClassificationScopeResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "s3")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetClassificationScope where
  hashWithSalt _salt GetClassificationScope' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetClassificationScope where
  rnf GetClassificationScope' {..} = Prelude.rnf id

instance Data.ToHeaders GetClassificationScope where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetClassificationScope where
  toPath GetClassificationScope' {..} =
    Prelude.mconcat
      ["/classification-scopes/", Data.toBS id]

instance Data.ToQuery GetClassificationScope where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClassificationScopeResponse' smart constructor.
data GetClassificationScopeResponse = GetClassificationScopeResponse'
  { -- | The unique identifier for the classification scope.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the classification scope.
    name :: Prelude.Maybe Prelude.Text,
    -- | The S3 buckets that are excluded from automated sensitive data
    -- discovery.
    s3 :: Prelude.Maybe S3ClassificationScope,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClassificationScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getClassificationScopeResponse_id' - The unique identifier for the classification scope.
--
-- 'name', 'getClassificationScopeResponse_name' - The name of the classification scope.
--
-- 's3', 'getClassificationScopeResponse_s3' - The S3 buckets that are excluded from automated sensitive data
-- discovery.
--
-- 'httpStatus', 'getClassificationScopeResponse_httpStatus' - The response's http status code.
newGetClassificationScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClassificationScopeResponse
newGetClassificationScopeResponse pHttpStatus_ =
  GetClassificationScopeResponse'
    { id =
        Prelude.Nothing,
      name = Prelude.Nothing,
      s3 = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the classification scope.
getClassificationScopeResponse_id :: Lens.Lens' GetClassificationScopeResponse (Prelude.Maybe Prelude.Text)
getClassificationScopeResponse_id = Lens.lens (\GetClassificationScopeResponse' {id} -> id) (\s@GetClassificationScopeResponse' {} a -> s {id = a} :: GetClassificationScopeResponse)

-- | The name of the classification scope.
getClassificationScopeResponse_name :: Lens.Lens' GetClassificationScopeResponse (Prelude.Maybe Prelude.Text)
getClassificationScopeResponse_name = Lens.lens (\GetClassificationScopeResponse' {name} -> name) (\s@GetClassificationScopeResponse' {} a -> s {name = a} :: GetClassificationScopeResponse)

-- | The S3 buckets that are excluded from automated sensitive data
-- discovery.
getClassificationScopeResponse_s3 :: Lens.Lens' GetClassificationScopeResponse (Prelude.Maybe S3ClassificationScope)
getClassificationScopeResponse_s3 = Lens.lens (\GetClassificationScopeResponse' {s3} -> s3) (\s@GetClassificationScopeResponse' {} a -> s {s3 = a} :: GetClassificationScopeResponse)

-- | The response's http status code.
getClassificationScopeResponse_httpStatus :: Lens.Lens' GetClassificationScopeResponse Prelude.Int
getClassificationScopeResponse_httpStatus = Lens.lens (\GetClassificationScopeResponse' {httpStatus} -> httpStatus) (\s@GetClassificationScopeResponse' {} a -> s {httpStatus = a} :: GetClassificationScopeResponse)

instance
  Prelude.NFData
    GetClassificationScopeResponse
  where
  rnf GetClassificationScopeResponse' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf httpStatus
