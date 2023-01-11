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
-- Module      : Amazonka.SageMaker.CreateModelCard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker Model Card.
--
-- For information about how to use model cards, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards.html Amazon SageMaker Model Card>.
module Amazonka.SageMaker.CreateModelCard
  ( -- * Creating a Request
    CreateModelCard (..),
    newCreateModelCard,

    -- * Request Lenses
    createModelCard_securityConfig,
    createModelCard_tags,
    createModelCard_modelCardName,
    createModelCard_content,
    createModelCard_modelCardStatus,

    -- * Destructuring the Response
    CreateModelCardResponse (..),
    newCreateModelCardResponse,

    -- * Response Lenses
    createModelCardResponse_httpStatus,
    createModelCardResponse_modelCardArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateModelCard' smart constructor.
data CreateModelCard = CreateModelCard'
  { -- | An optional Key Management Service key to encrypt, decrypt, and
    -- re-encrypt model card content for regulated workloads with highly
    -- sensitive data.
    securityConfig :: Prelude.Maybe ModelCardSecurityConfig,
    -- | Key-value pairs used to manage metadata for model cards.
    tags :: Prelude.Maybe [Tag],
    -- | The unique name of the model card.
    modelCardName :: Prelude.Text,
    -- | The content of the model card. Content must be in
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
    -- and provided as a string.
    content :: Data.Sensitive Prelude.Text,
    -- | The approval status of the model card within your organization.
    -- Different organizations might have different criteria for model card
    -- review and approval.
    --
    -- -   @Draft@: The model card is a work in progress.
    --
    -- -   @PendingReview@: The model card is pending review.
    --
    -- -   @Approved@: The model card is approved.
    --
    -- -   @Archived@: The model card is archived. No more updates should be
    --     made to the model card, but it can still be exported.
    modelCardStatus :: ModelCardStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfig', 'createModelCard_securityConfig' - An optional Key Management Service key to encrypt, decrypt, and
-- re-encrypt model card content for regulated workloads with highly
-- sensitive data.
--
-- 'tags', 'createModelCard_tags' - Key-value pairs used to manage metadata for model cards.
--
-- 'modelCardName', 'createModelCard_modelCardName' - The unique name of the model card.
--
-- 'content', 'createModelCard_content' - The content of the model card. Content must be in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
-- and provided as a string.
--
-- 'modelCardStatus', 'createModelCard_modelCardStatus' - The approval status of the model card within your organization.
-- Different organizations might have different criteria for model card
-- review and approval.
--
-- -   @Draft@: The model card is a work in progress.
--
-- -   @PendingReview@: The model card is pending review.
--
-- -   @Approved@: The model card is approved.
--
-- -   @Archived@: The model card is archived. No more updates should be
--     made to the model card, but it can still be exported.
newCreateModelCard ::
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  -- | 'modelCardStatus'
  ModelCardStatus ->
  CreateModelCard
newCreateModelCard
  pModelCardName_
  pContent_
  pModelCardStatus_ =
    CreateModelCard'
      { securityConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        modelCardName = pModelCardName_,
        content = Data._Sensitive Lens.# pContent_,
        modelCardStatus = pModelCardStatus_
      }

-- | An optional Key Management Service key to encrypt, decrypt, and
-- re-encrypt model card content for regulated workloads with highly
-- sensitive data.
createModelCard_securityConfig :: Lens.Lens' CreateModelCard (Prelude.Maybe ModelCardSecurityConfig)
createModelCard_securityConfig = Lens.lens (\CreateModelCard' {securityConfig} -> securityConfig) (\s@CreateModelCard' {} a -> s {securityConfig = a} :: CreateModelCard)

-- | Key-value pairs used to manage metadata for model cards.
createModelCard_tags :: Lens.Lens' CreateModelCard (Prelude.Maybe [Tag])
createModelCard_tags = Lens.lens (\CreateModelCard' {tags} -> tags) (\s@CreateModelCard' {} a -> s {tags = a} :: CreateModelCard) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the model card.
createModelCard_modelCardName :: Lens.Lens' CreateModelCard Prelude.Text
createModelCard_modelCardName = Lens.lens (\CreateModelCard' {modelCardName} -> modelCardName) (\s@CreateModelCard' {} a -> s {modelCardName = a} :: CreateModelCard)

-- | The content of the model card. Content must be in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
-- and provided as a string.
createModelCard_content :: Lens.Lens' CreateModelCard Prelude.Text
createModelCard_content = Lens.lens (\CreateModelCard' {content} -> content) (\s@CreateModelCard' {} a -> s {content = a} :: CreateModelCard) Prelude.. Data._Sensitive

-- | The approval status of the model card within your organization.
-- Different organizations might have different criteria for model card
-- review and approval.
--
-- -   @Draft@: The model card is a work in progress.
--
-- -   @PendingReview@: The model card is pending review.
--
-- -   @Approved@: The model card is approved.
--
-- -   @Archived@: The model card is archived. No more updates should be
--     made to the model card, but it can still be exported.
createModelCard_modelCardStatus :: Lens.Lens' CreateModelCard ModelCardStatus
createModelCard_modelCardStatus = Lens.lens (\CreateModelCard' {modelCardStatus} -> modelCardStatus) (\s@CreateModelCard' {} a -> s {modelCardStatus = a} :: CreateModelCard)

instance Core.AWSRequest CreateModelCard where
  type
    AWSResponse CreateModelCard =
      CreateModelCardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelCardResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelCardArn")
      )

instance Prelude.Hashable CreateModelCard where
  hashWithSalt _salt CreateModelCard' {..} =
    _salt `Prelude.hashWithSalt` securityConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelCardName
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` modelCardStatus

instance Prelude.NFData CreateModelCard where
  rnf CreateModelCard' {..} =
    Prelude.rnf securityConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelCardName
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf modelCardStatus

instance Data.ToHeaders CreateModelCard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateModelCard" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModelCard where
  toJSON CreateModelCard' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityConfig" Data..=)
              Prelude.<$> securityConfig,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ModelCardName" Data..= modelCardName),
            Prelude.Just ("Content" Data..= content),
            Prelude.Just
              ("ModelCardStatus" Data..= modelCardStatus)
          ]
      )

instance Data.ToPath CreateModelCard where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModelCard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelCardResponse' smart constructor.
data CreateModelCardResponse = CreateModelCardResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the successfully created model card.
    modelCardArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelCardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelCardResponse_httpStatus' - The response's http status code.
--
-- 'modelCardArn', 'createModelCardResponse_modelCardArn' - The Amazon Resource Name (ARN) of the successfully created model card.
newCreateModelCardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelCardArn'
  Prelude.Text ->
  CreateModelCardResponse
newCreateModelCardResponse
  pHttpStatus_
  pModelCardArn_ =
    CreateModelCardResponse'
      { httpStatus = pHttpStatus_,
        modelCardArn = pModelCardArn_
      }

-- | The response's http status code.
createModelCardResponse_httpStatus :: Lens.Lens' CreateModelCardResponse Prelude.Int
createModelCardResponse_httpStatus = Lens.lens (\CreateModelCardResponse' {httpStatus} -> httpStatus) (\s@CreateModelCardResponse' {} a -> s {httpStatus = a} :: CreateModelCardResponse)

-- | The Amazon Resource Name (ARN) of the successfully created model card.
createModelCardResponse_modelCardArn :: Lens.Lens' CreateModelCardResponse Prelude.Text
createModelCardResponse_modelCardArn = Lens.lens (\CreateModelCardResponse' {modelCardArn} -> modelCardArn) (\s@CreateModelCardResponse' {} a -> s {modelCardArn = a} :: CreateModelCardResponse)

instance Prelude.NFData CreateModelCardResponse where
  rnf CreateModelCardResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardArn
