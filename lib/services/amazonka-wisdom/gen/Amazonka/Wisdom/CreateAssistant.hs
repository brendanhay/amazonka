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
-- Module      : Amazonka.Wisdom.CreateAssistant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Connect Wisdom assistant.
module Amazonka.Wisdom.CreateAssistant
  ( -- * Creating a Request
    CreateAssistant (..),
    newCreateAssistant,

    -- * Request Lenses
    createAssistant_clientToken,
    createAssistant_description,
    createAssistant_serverSideEncryptionConfiguration,
    createAssistant_tags,
    createAssistant_name,
    createAssistant_type,

    -- * Destructuring the Response
    CreateAssistantResponse (..),
    newCreateAssistantResponse,

    -- * Response Lenses
    createAssistantResponse_assistant,
    createAssistantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newCreateAssistant' smart constructor.
data CreateAssistant = CreateAssistant'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the assistant.
    description :: Prelude.Maybe Prelude.Text,
    -- | The KMS key used for encryption.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the assistant.
    name :: Prelude.Text,
    -- | The type of assistant.
    type' :: AssistantType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssistant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createAssistant_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'description', 'createAssistant_description' - The description of the assistant.
--
-- 'serverSideEncryptionConfiguration', 'createAssistant_serverSideEncryptionConfiguration' - The KMS key used for encryption.
--
-- 'tags', 'createAssistant_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'name', 'createAssistant_name' - The name of the assistant.
--
-- 'type'', 'createAssistant_type' - The type of assistant.
newCreateAssistant ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  AssistantType ->
  CreateAssistant
newCreateAssistant pName_ pType_ =
  CreateAssistant'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
createAssistant_clientToken :: Lens.Lens' CreateAssistant (Prelude.Maybe Prelude.Text)
createAssistant_clientToken = Lens.lens (\CreateAssistant' {clientToken} -> clientToken) (\s@CreateAssistant' {} a -> s {clientToken = a} :: CreateAssistant)

-- | The description of the assistant.
createAssistant_description :: Lens.Lens' CreateAssistant (Prelude.Maybe Prelude.Text)
createAssistant_description = Lens.lens (\CreateAssistant' {description} -> description) (\s@CreateAssistant' {} a -> s {description = a} :: CreateAssistant)

-- | The KMS key used for encryption.
createAssistant_serverSideEncryptionConfiguration :: Lens.Lens' CreateAssistant (Prelude.Maybe ServerSideEncryptionConfiguration)
createAssistant_serverSideEncryptionConfiguration = Lens.lens (\CreateAssistant' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@CreateAssistant' {} a -> s {serverSideEncryptionConfiguration = a} :: CreateAssistant)

-- | The tags used to organize, track, or control access for this resource.
createAssistant_tags :: Lens.Lens' CreateAssistant (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAssistant_tags = Lens.lens (\CreateAssistant' {tags} -> tags) (\s@CreateAssistant' {} a -> s {tags = a} :: CreateAssistant) Prelude.. Lens.mapping Lens.coerced

-- | The name of the assistant.
createAssistant_name :: Lens.Lens' CreateAssistant Prelude.Text
createAssistant_name = Lens.lens (\CreateAssistant' {name} -> name) (\s@CreateAssistant' {} a -> s {name = a} :: CreateAssistant)

-- | The type of assistant.
createAssistant_type :: Lens.Lens' CreateAssistant AssistantType
createAssistant_type = Lens.lens (\CreateAssistant' {type'} -> type') (\s@CreateAssistant' {} a -> s {type' = a} :: CreateAssistant)

instance Core.AWSRequest CreateAssistant where
  type
    AWSResponse CreateAssistant =
      CreateAssistantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssistantResponse'
            Prelude.<$> (x Data..?> "assistant")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssistant where
  hashWithSalt _salt CreateAssistant' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateAssistant where
  rnf CreateAssistant' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateAssistant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssistant where
  toJSON CreateAssistant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("serverSideEncryptionConfiguration" Data..=)
              Prelude.<$> serverSideEncryptionConfiguration,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateAssistant where
  toPath = Prelude.const "/assistants"

instance Data.ToQuery CreateAssistant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssistantResponse' smart constructor.
data CreateAssistantResponse = CreateAssistantResponse'
  { -- | Information about the assistant.
    assistant :: Prelude.Maybe AssistantData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssistantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistant', 'createAssistantResponse_assistant' - Information about the assistant.
--
-- 'httpStatus', 'createAssistantResponse_httpStatus' - The response's http status code.
newCreateAssistantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssistantResponse
newCreateAssistantResponse pHttpStatus_ =
  CreateAssistantResponse'
    { assistant =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the assistant.
createAssistantResponse_assistant :: Lens.Lens' CreateAssistantResponse (Prelude.Maybe AssistantData)
createAssistantResponse_assistant = Lens.lens (\CreateAssistantResponse' {assistant} -> assistant) (\s@CreateAssistantResponse' {} a -> s {assistant = a} :: CreateAssistantResponse)

-- | The response's http status code.
createAssistantResponse_httpStatus :: Lens.Lens' CreateAssistantResponse Prelude.Int
createAssistantResponse_httpStatus = Lens.lens (\CreateAssistantResponse' {httpStatus} -> httpStatus) (\s@CreateAssistantResponse' {} a -> s {httpStatus = a} :: CreateAssistantResponse)

instance Prelude.NFData CreateAssistantResponse where
  rnf CreateAssistantResponse' {..} =
    Prelude.rnf assistant
      `Prelude.seq` Prelude.rnf httpStatus
