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
-- Module      : Amazonka.VoiceId.CreateDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain that contains all Amazon Connect Voice ID data, such as
-- speakers, fraudsters, customer audio, and voiceprints.
module Amazonka.VoiceId.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_tags,
    createDomain_clientToken,
    createDomain_description,
    createDomain_name,
    createDomain_serverSideEncryptionConfiguration,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_domain,
    createDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | A list of tags you want added to the domain.
    tags :: Prelude.Maybe [Tag],
    -- | The idempotency token for creating a new domain. If not provided, Amazon
    -- Web Services SDK populates this field.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the domain.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the domain.
    name :: Data.Sensitive Prelude.Text,
    -- | The configuration, containing the KMS key identifier, to be used by
    -- Voice ID for the server-side encryption of your data. Refer to
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/encryption-at-rest.html#encryption-at-rest-voiceid Amazon Connect Voice ID encryption at rest>
    -- for more details on how the KMS key is used.
    serverSideEncryptionConfiguration :: ServerSideEncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDomain_tags' - A list of tags you want added to the domain.
--
-- 'clientToken', 'createDomain_clientToken' - The idempotency token for creating a new domain. If not provided, Amazon
-- Web Services SDK populates this field.
--
-- 'description', 'createDomain_description' - A brief description of the domain.
--
-- 'name', 'createDomain_name' - The name of the domain.
--
-- 'serverSideEncryptionConfiguration', 'createDomain_serverSideEncryptionConfiguration' - The configuration, containing the KMS key identifier, to be used by
-- Voice ID for the server-side encryption of your data. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/encryption-at-rest.html#encryption-at-rest-voiceid Amazon Connect Voice ID encryption at rest>
-- for more details on how the KMS key is used.
newCreateDomain ::
  -- | 'name'
  Prelude.Text ->
  -- | 'serverSideEncryptionConfiguration'
  ServerSideEncryptionConfiguration ->
  CreateDomain
newCreateDomain
  pName_
  pServerSideEncryptionConfiguration_ =
    CreateDomain'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        name = Data._Sensitive Lens.# pName_,
        serverSideEncryptionConfiguration =
          pServerSideEncryptionConfiguration_
      }

-- | A list of tags you want added to the domain.
createDomain_tags :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tags = Lens.lens (\CreateDomain' {tags} -> tags) (\s@CreateDomain' {} a -> s {tags = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | The idempotency token for creating a new domain. If not provided, Amazon
-- Web Services SDK populates this field.
createDomain_clientToken :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_clientToken = Lens.lens (\CreateDomain' {clientToken} -> clientToken) (\s@CreateDomain' {} a -> s {clientToken = a} :: CreateDomain)

-- | A brief description of the domain.
createDomain_description :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_description = Lens.lens (\CreateDomain' {description} -> description) (\s@CreateDomain' {} a -> s {description = a} :: CreateDomain) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the domain.
createDomain_name :: Lens.Lens' CreateDomain Prelude.Text
createDomain_name = Lens.lens (\CreateDomain' {name} -> name) (\s@CreateDomain' {} a -> s {name = a} :: CreateDomain) Prelude.. Data._Sensitive

-- | The configuration, containing the KMS key identifier, to be used by
-- Voice ID for the server-side encryption of your data. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/encryption-at-rest.html#encryption-at-rest-voiceid Amazon Connect Voice ID encryption at rest>
-- for more details on how the KMS key is used.
createDomain_serverSideEncryptionConfiguration :: Lens.Lens' CreateDomain ServerSideEncryptionConfiguration
createDomain_serverSideEncryptionConfiguration = Lens.lens (\CreateDomain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@CreateDomain' {} a -> s {serverSideEncryptionConfiguration = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Data..?> "Domain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain where
  hashWithSalt _salt CreateDomain' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration

instance Prelude.NFData CreateDomain where
  rnf CreateDomain' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration

instance Data.ToHeaders CreateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.CreateDomain" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "ServerSideEncryptionConfiguration"
                  Data..= serverSideEncryptionConfiguration
              )
          ]
      )

instance Data.ToPath CreateDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | Information about the newly created domain.
    domain :: Prelude.Maybe Domain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createDomainResponse_domain' - Information about the newly created domain.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainResponse
newCreateDomainResponse pHttpStatus_ =
  CreateDomainResponse'
    { domain = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the newly created domain.
createDomainResponse_domain :: Lens.Lens' CreateDomainResponse (Prelude.Maybe Domain)
createDomainResponse_domain = Lens.lens (\CreateDomainResponse' {domain} -> domain) (\s@CreateDomainResponse' {} a -> s {domain = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

instance Prelude.NFData CreateDomainResponse where
  rnf CreateDomainResponse' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf httpStatus
