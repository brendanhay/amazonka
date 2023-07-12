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
-- Module      : Amazonka.ConnectCases.GetDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain if it exists.
module Amazonka.ConnectCases.GetDomain
  ( -- * Creating a Request
    GetDomain (..),
    newGetDomain,

    -- * Request Lenses
    getDomain_domainId,

    -- * Destructuring the Response
    GetDomainResponse (..),
    newGetDomainResponse,

    -- * Response Lenses
    getDomainResponse_tags,
    getDomainResponse_httpStatus,
    getDomainResponse_createdTime,
    getDomainResponse_domainArn,
    getDomainResponse_domainId,
    getDomainResponse_domainStatus,
    getDomainResponse_name,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomain' smart constructor.
data GetDomain = GetDomain'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'getDomain_domainId' - The unique identifier of the Cases domain.
newGetDomain ::
  -- | 'domainId'
  Prelude.Text ->
  GetDomain
newGetDomain pDomainId_ =
  GetDomain' {domainId = pDomainId_}

-- | The unique identifier of the Cases domain.
getDomain_domainId :: Lens.Lens' GetDomain Prelude.Text
getDomain_domainId = Lens.lens (\GetDomain' {domainId} -> domainId) (\s@GetDomain' {} a -> s {domainId = a} :: GetDomain)

instance Core.AWSRequest GetDomain where
  type AWSResponse GetDomain = GetDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "createdTime")
            Prelude.<*> (x Data..:> "domainArn")
            Prelude.<*> (x Data..:> "domainId")
            Prelude.<*> (x Data..:> "domainStatus")
            Prelude.<*> (x Data..:> "name")
      )

instance Prelude.Hashable GetDomain where
  hashWithSalt _salt GetDomain' {..} =
    _salt `Prelude.hashWithSalt` domainId

instance Prelude.NFData GetDomain where
  rnf GetDomain' {..} = Prelude.rnf domainId

instance Data.ToHeaders GetDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDomain where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetDomain where
  toPath GetDomain' {..} =
    Prelude.mconcat ["/domains/", Data.toBS domainId]

instance Data.ToQuery GetDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp when the Cases domain was created.
    createdTime :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) for the Cases domain.
    domainArn :: Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The status of the Cases domain.
    domainStatus :: DomainStatus,
    -- | The name of the Cases domain.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getDomainResponse_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'getDomainResponse_httpStatus' - The response's http status code.
--
-- 'createdTime', 'getDomainResponse_createdTime' - The timestamp when the Cases domain was created.
--
-- 'domainArn', 'getDomainResponse_domainArn' - The Amazon Resource Name (ARN) for the Cases domain.
--
-- 'domainId', 'getDomainResponse_domainId' - The unique identifier of the Cases domain.
--
-- 'domainStatus', 'getDomainResponse_domainStatus' - The status of the Cases domain.
--
-- 'name', 'getDomainResponse_name' - The name of the Cases domain.
newGetDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'domainArn'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'domainStatus'
  DomainStatus ->
  -- | 'name'
  Prelude.Text ->
  GetDomainResponse
newGetDomainResponse
  pHttpStatus_
  pCreatedTime_
  pDomainArn_
  pDomainId_
  pDomainStatus_
  pName_ =
    GetDomainResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        domainArn = pDomainArn_,
        domainId = pDomainId_,
        domainStatus = pDomainStatus_,
        name = pName_
      }

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
getDomainResponse_tags :: Lens.Lens' GetDomainResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDomainResponse_tags = Lens.lens (\GetDomainResponse' {tags} -> tags) (\s@GetDomainResponse' {} a -> s {tags = a} :: GetDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDomainResponse_httpStatus :: Lens.Lens' GetDomainResponse Prelude.Int
getDomainResponse_httpStatus = Lens.lens (\GetDomainResponse' {httpStatus} -> httpStatus) (\s@GetDomainResponse' {} a -> s {httpStatus = a} :: GetDomainResponse)

-- | The timestamp when the Cases domain was created.
getDomainResponse_createdTime :: Lens.Lens' GetDomainResponse Prelude.UTCTime
getDomainResponse_createdTime = Lens.lens (\GetDomainResponse' {createdTime} -> createdTime) (\s@GetDomainResponse' {} a -> s {createdTime = a} :: GetDomainResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) for the Cases domain.
getDomainResponse_domainArn :: Lens.Lens' GetDomainResponse Prelude.Text
getDomainResponse_domainArn = Lens.lens (\GetDomainResponse' {domainArn} -> domainArn) (\s@GetDomainResponse' {} a -> s {domainArn = a} :: GetDomainResponse)

-- | The unique identifier of the Cases domain.
getDomainResponse_domainId :: Lens.Lens' GetDomainResponse Prelude.Text
getDomainResponse_domainId = Lens.lens (\GetDomainResponse' {domainId} -> domainId) (\s@GetDomainResponse' {} a -> s {domainId = a} :: GetDomainResponse)

-- | The status of the Cases domain.
getDomainResponse_domainStatus :: Lens.Lens' GetDomainResponse DomainStatus
getDomainResponse_domainStatus = Lens.lens (\GetDomainResponse' {domainStatus} -> domainStatus) (\s@GetDomainResponse' {} a -> s {domainStatus = a} :: GetDomainResponse)

-- | The name of the Cases domain.
getDomainResponse_name :: Lens.Lens' GetDomainResponse Prelude.Text
getDomainResponse_name = Lens.lens (\GetDomainResponse' {name} -> name) (\s@GetDomainResponse' {} a -> s {name = a} :: GetDomainResponse)

instance Prelude.NFData GetDomainResponse where
  rnf GetDomainResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf domainArn
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf name
