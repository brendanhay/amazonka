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
-- Module      : Amazonka.Wisdom.CreateAssistantAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an Amazon Connect Wisdom assistant and
-- another resource. Currently, the only supported association is with a
-- knowledge base. An assistant can have only a single association.
module Amazonka.Wisdom.CreateAssistantAssociation
  ( -- * Creating a Request
    CreateAssistantAssociation (..),
    newCreateAssistantAssociation,

    -- * Request Lenses
    createAssistantAssociation_clientToken,
    createAssistantAssociation_tags,
    createAssistantAssociation_assistantId,
    createAssistantAssociation_association,
    createAssistantAssociation_associationType,

    -- * Destructuring the Response
    CreateAssistantAssociationResponse (..),
    newCreateAssistantAssociationResponse,

    -- * Response Lenses
    createAssistantAssociationResponse_assistantAssociation,
    createAssistantAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newCreateAssistantAssociation' smart constructor.
data CreateAssistantAssociation = CreateAssistantAssociation'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The identifier of the associated resource.
    association :: AssistantAssociationInputData,
    -- | The type of association.
    associationType :: AssociationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssistantAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createAssistantAssociation_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'tags', 'createAssistantAssociation_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'assistantId', 'createAssistantAssociation_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'association', 'createAssistantAssociation_association' - The identifier of the associated resource.
--
-- 'associationType', 'createAssistantAssociation_associationType' - The type of association.
newCreateAssistantAssociation ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'association'
  AssistantAssociationInputData ->
  -- | 'associationType'
  AssociationType ->
  CreateAssistantAssociation
newCreateAssistantAssociation
  pAssistantId_
  pAssociation_
  pAssociationType_ =
    CreateAssistantAssociation'
      { clientToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        assistantId = pAssistantId_,
        association = pAssociation_,
        associationType = pAssociationType_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createAssistantAssociation_clientToken :: Lens.Lens' CreateAssistantAssociation (Prelude.Maybe Prelude.Text)
createAssistantAssociation_clientToken = Lens.lens (\CreateAssistantAssociation' {clientToken} -> clientToken) (\s@CreateAssistantAssociation' {} a -> s {clientToken = a} :: CreateAssistantAssociation)

-- | The tags used to organize, track, or control access for this resource.
createAssistantAssociation_tags :: Lens.Lens' CreateAssistantAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAssistantAssociation_tags = Lens.lens (\CreateAssistantAssociation' {tags} -> tags) (\s@CreateAssistantAssociation' {} a -> s {tags = a} :: CreateAssistantAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
createAssistantAssociation_assistantId :: Lens.Lens' CreateAssistantAssociation Prelude.Text
createAssistantAssociation_assistantId = Lens.lens (\CreateAssistantAssociation' {assistantId} -> assistantId) (\s@CreateAssistantAssociation' {} a -> s {assistantId = a} :: CreateAssistantAssociation)

-- | The identifier of the associated resource.
createAssistantAssociation_association :: Lens.Lens' CreateAssistantAssociation AssistantAssociationInputData
createAssistantAssociation_association = Lens.lens (\CreateAssistantAssociation' {association} -> association) (\s@CreateAssistantAssociation' {} a -> s {association = a} :: CreateAssistantAssociation)

-- | The type of association.
createAssistantAssociation_associationType :: Lens.Lens' CreateAssistantAssociation AssociationType
createAssistantAssociation_associationType = Lens.lens (\CreateAssistantAssociation' {associationType} -> associationType) (\s@CreateAssistantAssociation' {} a -> s {associationType = a} :: CreateAssistantAssociation)

instance Core.AWSRequest CreateAssistantAssociation where
  type
    AWSResponse CreateAssistantAssociation =
      CreateAssistantAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssistantAssociationResponse'
            Prelude.<$> (x Data..?> "assistantAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssistantAssociation where
  hashWithSalt _salt CreateAssistantAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` association
      `Prelude.hashWithSalt` associationType

instance Prelude.NFData CreateAssistantAssociation where
  rnf CreateAssistantAssociation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf association
      `Prelude.seq` Prelude.rnf associationType

instance Data.ToHeaders CreateAssistantAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssistantAssociation where
  toJSON CreateAssistantAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("association" Data..= association),
            Prelude.Just
              ("associationType" Data..= associationType)
          ]
      )

instance Data.ToPath CreateAssistantAssociation where
  toPath CreateAssistantAssociation' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Data.toBS assistantId,
        "/associations"
      ]

instance Data.ToQuery CreateAssistantAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssistantAssociationResponse' smart constructor.
data CreateAssistantAssociationResponse = CreateAssistantAssociationResponse'
  { -- | The assistant association.
    assistantAssociation :: Prelude.Maybe AssistantAssociationData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssistantAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantAssociation', 'createAssistantAssociationResponse_assistantAssociation' - The assistant association.
--
-- 'httpStatus', 'createAssistantAssociationResponse_httpStatus' - The response's http status code.
newCreateAssistantAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssistantAssociationResponse
newCreateAssistantAssociationResponse pHttpStatus_ =
  CreateAssistantAssociationResponse'
    { assistantAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The assistant association.
createAssistantAssociationResponse_assistantAssociation :: Lens.Lens' CreateAssistantAssociationResponse (Prelude.Maybe AssistantAssociationData)
createAssistantAssociationResponse_assistantAssociation = Lens.lens (\CreateAssistantAssociationResponse' {assistantAssociation} -> assistantAssociation) (\s@CreateAssistantAssociationResponse' {} a -> s {assistantAssociation = a} :: CreateAssistantAssociationResponse)

-- | The response's http status code.
createAssistantAssociationResponse_httpStatus :: Lens.Lens' CreateAssistantAssociationResponse Prelude.Int
createAssistantAssociationResponse_httpStatus = Lens.lens (\CreateAssistantAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateAssistantAssociationResponse' {} a -> s {httpStatus = a} :: CreateAssistantAssociationResponse)

instance
  Prelude.NFData
    CreateAssistantAssociationResponse
  where
  rnf CreateAssistantAssociationResponse' {..} =
    Prelude.rnf assistantAssociation
      `Prelude.seq` Prelude.rnf httpStatus
