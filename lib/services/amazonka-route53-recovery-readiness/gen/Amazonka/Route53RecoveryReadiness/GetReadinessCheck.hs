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
-- Module      : Amazonka.Route53RecoveryReadiness.GetReadinessCheck
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a readiness check.
module Amazonka.Route53RecoveryReadiness.GetReadinessCheck
  ( -- * Creating a Request
    GetReadinessCheck (..),
    newGetReadinessCheck,

    -- * Request Lenses
    getReadinessCheck_readinessCheckName,

    -- * Destructuring the Response
    GetReadinessCheckResponse (..),
    newGetReadinessCheckResponse,

    -- * Response Lenses
    getReadinessCheckResponse_readinessCheckArn,
    getReadinessCheckResponse_readinessCheckName,
    getReadinessCheckResponse_resourceSet,
    getReadinessCheckResponse_tags,
    getReadinessCheckResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetReadinessCheck' smart constructor.
data GetReadinessCheck = GetReadinessCheck'
  { -- | Name of a readiness check.
    readinessCheckName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadinessCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessCheckName', 'getReadinessCheck_readinessCheckName' - Name of a readiness check.
newGetReadinessCheck ::
  -- | 'readinessCheckName'
  Prelude.Text ->
  GetReadinessCheck
newGetReadinessCheck pReadinessCheckName_ =
  GetReadinessCheck'
    { readinessCheckName =
        pReadinessCheckName_
    }

-- | Name of a readiness check.
getReadinessCheck_readinessCheckName :: Lens.Lens' GetReadinessCheck Prelude.Text
getReadinessCheck_readinessCheckName = Lens.lens (\GetReadinessCheck' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheck' {} a -> s {readinessCheckName = a} :: GetReadinessCheck)

instance Core.AWSRequest GetReadinessCheck where
  type
    AWSResponse GetReadinessCheck =
      GetReadinessCheckResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadinessCheckResponse'
            Prelude.<$> (x Data..?> "readinessCheckArn")
            Prelude.<*> (x Data..?> "readinessCheckName")
            Prelude.<*> (x Data..?> "resourceSet")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetReadinessCheck where
  hashWithSalt _salt GetReadinessCheck' {..} =
    _salt `Prelude.hashWithSalt` readinessCheckName

instance Prelude.NFData GetReadinessCheck where
  rnf GetReadinessCheck' {..} =
    Prelude.rnf readinessCheckName

instance Data.ToHeaders GetReadinessCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadinessCheck where
  toPath GetReadinessCheck' {..} =
    Prelude.mconcat
      ["/readinesschecks/", Data.toBS readinessCheckName]

instance Data.ToQuery GetReadinessCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReadinessCheckResponse' smart constructor.
data GetReadinessCheckResponse = GetReadinessCheckResponse'
  { -- | The Amazon Resource Name (ARN) associated with a readiness check.
    readinessCheckArn :: Prelude.Maybe Prelude.Text,
    -- | Name of a readiness check.
    readinessCheckName :: Prelude.Maybe Prelude.Text,
    -- | Name of the resource set to be checked.
    resourceSet :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadinessCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessCheckArn', 'getReadinessCheckResponse_readinessCheckArn' - The Amazon Resource Name (ARN) associated with a readiness check.
--
-- 'readinessCheckName', 'getReadinessCheckResponse_readinessCheckName' - Name of a readiness check.
--
-- 'resourceSet', 'getReadinessCheckResponse_resourceSet' - Name of the resource set to be checked.
--
-- 'tags', 'getReadinessCheckResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'getReadinessCheckResponse_httpStatus' - The response's http status code.
newGetReadinessCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReadinessCheckResponse
newGetReadinessCheckResponse pHttpStatus_ =
  GetReadinessCheckResponse'
    { readinessCheckArn =
        Prelude.Nothing,
      readinessCheckName = Prelude.Nothing,
      resourceSet = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) associated with a readiness check.
getReadinessCheckResponse_readinessCheckArn :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResponse_readinessCheckArn = Lens.lens (\GetReadinessCheckResponse' {readinessCheckArn} -> readinessCheckArn) (\s@GetReadinessCheckResponse' {} a -> s {readinessCheckArn = a} :: GetReadinessCheckResponse)

-- | Name of a readiness check.
getReadinessCheckResponse_readinessCheckName :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResponse_readinessCheckName = Lens.lens (\GetReadinessCheckResponse' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheckResponse' {} a -> s {readinessCheckName = a} :: GetReadinessCheckResponse)

-- | Name of the resource set to be checked.
getReadinessCheckResponse_resourceSet :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResponse_resourceSet = Lens.lens (\GetReadinessCheckResponse' {resourceSet} -> resourceSet) (\s@GetReadinessCheckResponse' {} a -> s {resourceSet = a} :: GetReadinessCheckResponse)

-- | Undocumented member.
getReadinessCheckResponse_tags :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getReadinessCheckResponse_tags = Lens.lens (\GetReadinessCheckResponse' {tags} -> tags) (\s@GetReadinessCheckResponse' {} a -> s {tags = a} :: GetReadinessCheckResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getReadinessCheckResponse_httpStatus :: Lens.Lens' GetReadinessCheckResponse Prelude.Int
getReadinessCheckResponse_httpStatus = Lens.lens (\GetReadinessCheckResponse' {httpStatus} -> httpStatus) (\s@GetReadinessCheckResponse' {} a -> s {httpStatus = a} :: GetReadinessCheckResponse)

instance Prelude.NFData GetReadinessCheckResponse where
  rnf GetReadinessCheckResponse' {..} =
    Prelude.rnf readinessCheckArn
      `Prelude.seq` Prelude.rnf readinessCheckName
      `Prelude.seq` Prelude.rnf resourceSet
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
