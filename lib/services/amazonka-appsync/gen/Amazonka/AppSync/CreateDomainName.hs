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
-- Module      : Amazonka.AppSync.CreateDomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom @DomainName@ object.
module Amazonka.AppSync.CreateDomainName
  ( -- * Creating a Request
    CreateDomainName (..),
    newCreateDomainName,

    -- * Request Lenses
    createDomainName_description,
    createDomainName_domainName,
    createDomainName_certificateArn,

    -- * Destructuring the Response
    CreateDomainNameResponse (..),
    newCreateDomainNameResponse,

    -- * Response Lenses
    createDomainNameResponse_domainNameConfig,
    createDomainNameResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { -- | A description of the @DomainName@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate. This can be an
    -- Certificate Manager (ACM) certificate or an Identity and Access
    -- Management (IAM) server certificate.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDomainName_description' - A description of the @DomainName@.
--
-- 'domainName', 'createDomainName_domainName' - The domain name.
--
-- 'certificateArn', 'createDomainName_certificateArn' - The Amazon Resource Name (ARN) of the certificate. This can be an
-- Certificate Manager (ACM) certificate or an Identity and Access
-- Management (IAM) server certificate.
newCreateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'certificateArn'
  Prelude.Text ->
  CreateDomainName
newCreateDomainName pDomainName_ pCertificateArn_ =
  CreateDomainName'
    { description = Prelude.Nothing,
      domainName = pDomainName_,
      certificateArn = pCertificateArn_
    }

-- | A description of the @DomainName@.
createDomainName_description :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_description = Lens.lens (\CreateDomainName' {description} -> description) (\s@CreateDomainName' {} a -> s {description = a} :: CreateDomainName)

-- | The domain name.
createDomainName_domainName :: Lens.Lens' CreateDomainName Prelude.Text
createDomainName_domainName = Lens.lens (\CreateDomainName' {domainName} -> domainName) (\s@CreateDomainName' {} a -> s {domainName = a} :: CreateDomainName)

-- | The Amazon Resource Name (ARN) of the certificate. This can be an
-- Certificate Manager (ACM) certificate or an Identity and Access
-- Management (IAM) server certificate.
createDomainName_certificateArn :: Lens.Lens' CreateDomainName Prelude.Text
createDomainName_certificateArn = Lens.lens (\CreateDomainName' {certificateArn} -> certificateArn) (\s@CreateDomainName' {} a -> s {certificateArn = a} :: CreateDomainName)

instance Core.AWSRequest CreateDomainName where
  type
    AWSResponse CreateDomainName =
      CreateDomainNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainNameResponse'
            Prelude.<$> (x Data..?> "domainNameConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomainName where
  hashWithSalt _salt CreateDomainName' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData CreateDomainName where
  rnf CreateDomainName' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf certificateArn

instance Data.ToHeaders CreateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomainName where
  toJSON CreateDomainName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("domainName" Data..= domainName),
            Prelude.Just
              ("certificateArn" Data..= certificateArn)
          ]
      )

instance Data.ToPath CreateDomainName where
  toPath = Prelude.const "/v1/domainnames"

instance Data.ToQuery CreateDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainNameResponse' smart constructor.
data CreateDomainNameResponse = CreateDomainNameResponse'
  { -- | The configuration for the @DomainName@.
    domainNameConfig :: Prelude.Maybe DomainNameConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNameConfig', 'createDomainNameResponse_domainNameConfig' - The configuration for the @DomainName@.
--
-- 'httpStatus', 'createDomainNameResponse_httpStatus' - The response's http status code.
newCreateDomainNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainNameResponse
newCreateDomainNameResponse pHttpStatus_ =
  CreateDomainNameResponse'
    { domainNameConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration for the @DomainName@.
createDomainNameResponse_domainNameConfig :: Lens.Lens' CreateDomainNameResponse (Prelude.Maybe DomainNameConfig)
createDomainNameResponse_domainNameConfig = Lens.lens (\CreateDomainNameResponse' {domainNameConfig} -> domainNameConfig) (\s@CreateDomainNameResponse' {} a -> s {domainNameConfig = a} :: CreateDomainNameResponse)

-- | The response's http status code.
createDomainNameResponse_httpStatus :: Lens.Lens' CreateDomainNameResponse Prelude.Int
createDomainNameResponse_httpStatus = Lens.lens (\CreateDomainNameResponse' {httpStatus} -> httpStatus) (\s@CreateDomainNameResponse' {} a -> s {httpStatus = a} :: CreateDomainNameResponse)

instance Prelude.NFData CreateDomainNameResponse where
  rnf CreateDomainNameResponse' {..} =
    Prelude.rnf domainNameConfig
      `Prelude.seq` Prelude.rnf httpStatus
