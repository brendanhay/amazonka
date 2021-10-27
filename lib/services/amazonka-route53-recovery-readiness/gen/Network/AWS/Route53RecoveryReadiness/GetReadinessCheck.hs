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
-- Module      : Network.AWS.Route53RecoveryReadiness.GetReadinessCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a ReadinessCheck.
module Network.AWS.Route53RecoveryReadiness.GetReadinessCheck
  ( -- * Creating a Request
    GetReadinessCheck (..),
    newGetReadinessCheck,

    -- * Request Lenses
    getReadinessCheck_readinessCheckName,

    -- * Destructuring the Response
    GetReadinessCheckResponse (..),
    newGetReadinessCheckResponse,

    -- * Response Lenses
    getReadinessCheckResponse_readinessCheckName,
    getReadinessCheckResponse_resourceSet,
    getReadinessCheckResponse_readinessCheckArn,
    getReadinessCheckResponse_tags,
    getReadinessCheckResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetReadinessCheck' smart constructor.
data GetReadinessCheck = GetReadinessCheck'
  { -- | The ReadinessCheck to get
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
-- 'readinessCheckName', 'getReadinessCheck_readinessCheckName' - The ReadinessCheck to get
newGetReadinessCheck ::
  -- | 'readinessCheckName'
  Prelude.Text ->
  GetReadinessCheck
newGetReadinessCheck pReadinessCheckName_ =
  GetReadinessCheck'
    { readinessCheckName =
        pReadinessCheckName_
    }

-- | The ReadinessCheck to get
getReadinessCheck_readinessCheckName :: Lens.Lens' GetReadinessCheck Prelude.Text
getReadinessCheck_readinessCheckName = Lens.lens (\GetReadinessCheck' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheck' {} a -> s {readinessCheckName = a} :: GetReadinessCheck)

instance Core.AWSRequest GetReadinessCheck where
  type
    AWSResponse GetReadinessCheck =
      GetReadinessCheckResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadinessCheckResponse'
            Prelude.<$> (x Core..?> "readinessCheckName")
            Prelude.<*> (x Core..?> "resourceSet")
            Prelude.<*> (x Core..?> "readinessCheckArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetReadinessCheck

instance Prelude.NFData GetReadinessCheck

instance Core.ToHeaders GetReadinessCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetReadinessCheck where
  toPath GetReadinessCheck' {..} =
    Prelude.mconcat
      ["/readinesschecks/", Core.toBS readinessCheckName]

instance Core.ToQuery GetReadinessCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReadinessCheckResponse' smart constructor.
data GetReadinessCheckResponse = GetReadinessCheckResponse'
  { -- | Name for a ReadinessCheck
    readinessCheckName :: Prelude.Maybe Prelude.Text,
    -- | Name of the ResourceSet to be checked
    resourceSet :: Prelude.Maybe Prelude.Text,
    -- | Arn associated with ReadinessCheck
    readinessCheckArn :: Prelude.Maybe Prelude.Text,
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
-- 'readinessCheckName', 'getReadinessCheckResponse_readinessCheckName' - Name for a ReadinessCheck
--
-- 'resourceSet', 'getReadinessCheckResponse_resourceSet' - Name of the ResourceSet to be checked
--
-- 'readinessCheckArn', 'getReadinessCheckResponse_readinessCheckArn' - Arn associated with ReadinessCheck
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
    { readinessCheckName =
        Prelude.Nothing,
      resourceSet = Prelude.Nothing,
      readinessCheckArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Name for a ReadinessCheck
getReadinessCheckResponse_readinessCheckName :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResponse_readinessCheckName = Lens.lens (\GetReadinessCheckResponse' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheckResponse' {} a -> s {readinessCheckName = a} :: GetReadinessCheckResponse)

-- | Name of the ResourceSet to be checked
getReadinessCheckResponse_resourceSet :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResponse_resourceSet = Lens.lens (\GetReadinessCheckResponse' {resourceSet} -> resourceSet) (\s@GetReadinessCheckResponse' {} a -> s {resourceSet = a} :: GetReadinessCheckResponse)

-- | Arn associated with ReadinessCheck
getReadinessCheckResponse_readinessCheckArn :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResponse_readinessCheckArn = Lens.lens (\GetReadinessCheckResponse' {readinessCheckArn} -> readinessCheckArn) (\s@GetReadinessCheckResponse' {} a -> s {readinessCheckArn = a} :: GetReadinessCheckResponse)

-- | Undocumented member.
getReadinessCheckResponse_tags :: Lens.Lens' GetReadinessCheckResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getReadinessCheckResponse_tags = Lens.lens (\GetReadinessCheckResponse' {tags} -> tags) (\s@GetReadinessCheckResponse' {} a -> s {tags = a} :: GetReadinessCheckResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getReadinessCheckResponse_httpStatus :: Lens.Lens' GetReadinessCheckResponse Prelude.Int
getReadinessCheckResponse_httpStatus = Lens.lens (\GetReadinessCheckResponse' {httpStatus} -> httpStatus) (\s@GetReadinessCheckResponse' {} a -> s {httpStatus = a} :: GetReadinessCheckResponse)

instance Prelude.NFData GetReadinessCheckResponse
