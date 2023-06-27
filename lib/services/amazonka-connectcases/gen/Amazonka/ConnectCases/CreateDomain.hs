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
-- Module      : Amazonka.ConnectCases.CreateDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain, which is a container for all case data, such as cases,
-- fields, templates and layouts. Each Amazon Connect instance can be
-- associated with only one Cases domain.
--
-- This will not associate your connect instance to Cases domain. Instead,
-- use the Amazon Connect
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateIntegrationAssociation.html CreateIntegrationAssociation>
-- API. You need specific IAM permissions to successfully associate the
-- Cases domain. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/required-permissions-iam-cases.html#onboard-cases-iam Onboard to Cases>.
module Amazonka.ConnectCases.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_name,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_httpStatus,
    createDomainResponse_domainArn,
    createDomainResponse_domainId,
    createDomainResponse_domainStatus,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | The name for your Cases domain. It must be unique for your Amazon Web
    -- Services account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createDomain_name' - The name for your Cases domain. It must be unique for your Amazon Web
-- Services account.
newCreateDomain ::
  -- | 'name'
  Prelude.Text ->
  CreateDomain
newCreateDomain pName_ = CreateDomain' {name = pName_}

-- | The name for your Cases domain. It must be unique for your Amazon Web
-- Services account.
createDomain_name :: Lens.Lens' CreateDomain Prelude.Text
createDomain_name = Lens.lens (\CreateDomain' {name} -> name) (\s@CreateDomain' {} a -> s {name = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "domainArn")
            Prelude.<*> (x Data..:> "domainId")
            Prelude.<*> (x Data..:> "domainStatus")
      )

instance Prelude.Hashable CreateDomain where
  hashWithSalt _salt CreateDomain' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CreateDomain where
  rnf CreateDomain' {..} = Prelude.rnf name

instance Data.ToHeaders CreateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath CreateDomain where
  toPath = Prelude.const "/domains"

instance Data.ToQuery CreateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the Cases domain.
    domainArn :: Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The status of the domain.
    domainStatus :: DomainStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
--
-- 'domainArn', 'createDomainResponse_domainArn' - The Amazon Resource Name (ARN) for the Cases domain.
--
-- 'domainId', 'createDomainResponse_domainId' - The unique identifier of the Cases domain.
--
-- 'domainStatus', 'createDomainResponse_domainStatus' - The status of the domain.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainArn'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'domainStatus'
  DomainStatus ->
  CreateDomainResponse
newCreateDomainResponse
  pHttpStatus_
  pDomainArn_
  pDomainId_
  pDomainStatus_ =
    CreateDomainResponse'
      { httpStatus = pHttpStatus_,
        domainArn = pDomainArn_,
        domainId = pDomainId_,
        domainStatus = pDomainStatus_
      }

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

-- | The Amazon Resource Name (ARN) for the Cases domain.
createDomainResponse_domainArn :: Lens.Lens' CreateDomainResponse Prelude.Text
createDomainResponse_domainArn = Lens.lens (\CreateDomainResponse' {domainArn} -> domainArn) (\s@CreateDomainResponse' {} a -> s {domainArn = a} :: CreateDomainResponse)

-- | The unique identifier of the Cases domain.
createDomainResponse_domainId :: Lens.Lens' CreateDomainResponse Prelude.Text
createDomainResponse_domainId = Lens.lens (\CreateDomainResponse' {domainId} -> domainId) (\s@CreateDomainResponse' {} a -> s {domainId = a} :: CreateDomainResponse)

-- | The status of the domain.
createDomainResponse_domainStatus :: Lens.Lens' CreateDomainResponse DomainStatus
createDomainResponse_domainStatus = Lens.lens (\CreateDomainResponse' {domainStatus} -> domainStatus) (\s@CreateDomainResponse' {} a -> s {domainStatus = a} :: CreateDomainResponse)

instance Prelude.NFData CreateDomainResponse where
  rnf CreateDomainResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainArn
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf domainStatus
