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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createDomain_clientToken,
    createDomain_description,
    createDomain_tags,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | The idempotency token for creating a new domain. If not provided, Amazon
    -- Web Services SDK populates this field.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A brief description of this domain.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A list of tags you want added to the domain.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the domain.
    name :: Core.Sensitive Prelude.Text,
    -- | The configuration, containing the KMS Key Identifier, to be used by
    -- Voice ID for the server-side encryption of your data. Refer to
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/encryption-at-rest.html#encryption-at-rest-voiceid Amazon Connect VoiceID encryption at rest>
    -- for more details on how the KMS Key is used.
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
-- 'clientToken', 'createDomain_clientToken' - The idempotency token for creating a new domain. If not provided, Amazon
-- Web Services SDK populates this field.
--
-- 'description', 'createDomain_description' - A brief description of this domain.
--
-- 'tags', 'createDomain_tags' - A list of tags you want added to the domain.
--
-- 'name', 'createDomain_name' - The name of the domain.
--
-- 'serverSideEncryptionConfiguration', 'createDomain_serverSideEncryptionConfiguration' - The configuration, containing the KMS Key Identifier, to be used by
-- Voice ID for the server-side encryption of your data. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/encryption-at-rest.html#encryption-at-rest-voiceid Amazon Connect VoiceID encryption at rest>
-- for more details on how the KMS Key is used.
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
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = Core._Sensitive Lens.# pName_,
        serverSideEncryptionConfiguration =
          pServerSideEncryptionConfiguration_
      }

-- | The idempotency token for creating a new domain. If not provided, Amazon
-- Web Services SDK populates this field.
createDomain_clientToken :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_clientToken = Lens.lens (\CreateDomain' {clientToken} -> clientToken) (\s@CreateDomain' {} a -> s {clientToken = a} :: CreateDomain)

-- | A brief description of this domain.
createDomain_description :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_description = Lens.lens (\CreateDomain' {description} -> description) (\s@CreateDomain' {} a -> s {description = a} :: CreateDomain) Prelude.. Lens.mapping Core._Sensitive

-- | A list of tags you want added to the domain.
createDomain_tags :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tags = Lens.lens (\CreateDomain' {tags} -> tags) (\s@CreateDomain' {} a -> s {tags = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain.
createDomain_name :: Lens.Lens' CreateDomain Prelude.Text
createDomain_name = Lens.lens (\CreateDomain' {name} -> name) (\s@CreateDomain' {} a -> s {name = a} :: CreateDomain) Prelude.. Core._Sensitive

-- | The configuration, containing the KMS Key Identifier, to be used by
-- Voice ID for the server-side encryption of your data. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/encryption-at-rest.html#encryption-at-rest-voiceid Amazon Connect VoiceID encryption at rest>
-- for more details on how the KMS Key is used.
createDomain_serverSideEncryptionConfiguration :: Lens.Lens' CreateDomain ServerSideEncryptionConfiguration
createDomain_serverSideEncryptionConfiguration = Lens.lens (\CreateDomain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@CreateDomain' {} a -> s {serverSideEncryptionConfiguration = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Core..?> "Domain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain where
  hashWithSalt salt' CreateDomain' {..} =
    salt'
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateDomain where
  rnf CreateDomain' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders CreateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("VoiceID.CreateDomain" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ( "ServerSideEncryptionConfiguration"
                  Core..= serverSideEncryptionConfiguration
              )
          ]
      )

instance Core.ToPath CreateDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDomain where
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
