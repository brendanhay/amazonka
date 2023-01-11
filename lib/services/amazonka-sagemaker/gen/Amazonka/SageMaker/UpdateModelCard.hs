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
-- Module      : Amazonka.SageMaker.UpdateModelCard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Amazon SageMaker Model Card.
--
-- You cannot update both model card content and model card status in a
-- single call.
module Amazonka.SageMaker.UpdateModelCard
  ( -- * Creating a Request
    UpdateModelCard (..),
    newUpdateModelCard,

    -- * Request Lenses
    updateModelCard_content,
    updateModelCard_modelCardStatus,
    updateModelCard_modelCardName,

    -- * Destructuring the Response
    UpdateModelCardResponse (..),
    newUpdateModelCardResponse,

    -- * Response Lenses
    updateModelCardResponse_httpStatus,
    updateModelCardResponse_modelCardArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateModelCard' smart constructor.
data UpdateModelCard = UpdateModelCard'
  { -- | The updated model card content. Content must be in
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
    -- and provided as a string.
    --
    -- When updating model card content, be sure to include the full content
    -- and not just updated content.
    content :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
    modelCardStatus :: Prelude.Maybe ModelCardStatus,
    -- | The name of the model card to update.
    modelCardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'updateModelCard_content' - The updated model card content. Content must be in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
-- and provided as a string.
--
-- When updating model card content, be sure to include the full content
-- and not just updated content.
--
-- 'modelCardStatus', 'updateModelCard_modelCardStatus' - The approval status of the model card within your organization.
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
--
-- 'modelCardName', 'updateModelCard_modelCardName' - The name of the model card to update.
newUpdateModelCard ::
  -- | 'modelCardName'
  Prelude.Text ->
  UpdateModelCard
newUpdateModelCard pModelCardName_ =
  UpdateModelCard'
    { content = Prelude.Nothing,
      modelCardStatus = Prelude.Nothing,
      modelCardName = pModelCardName_
    }

-- | The updated model card content. Content must be in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
-- and provided as a string.
--
-- When updating model card content, be sure to include the full content
-- and not just updated content.
updateModelCard_content :: Lens.Lens' UpdateModelCard (Prelude.Maybe Prelude.Text)
updateModelCard_content = Lens.lens (\UpdateModelCard' {content} -> content) (\s@UpdateModelCard' {} a -> s {content = a} :: UpdateModelCard) Prelude.. Lens.mapping Data._Sensitive

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
updateModelCard_modelCardStatus :: Lens.Lens' UpdateModelCard (Prelude.Maybe ModelCardStatus)
updateModelCard_modelCardStatus = Lens.lens (\UpdateModelCard' {modelCardStatus} -> modelCardStatus) (\s@UpdateModelCard' {} a -> s {modelCardStatus = a} :: UpdateModelCard)

-- | The name of the model card to update.
updateModelCard_modelCardName :: Lens.Lens' UpdateModelCard Prelude.Text
updateModelCard_modelCardName = Lens.lens (\UpdateModelCard' {modelCardName} -> modelCardName) (\s@UpdateModelCard' {} a -> s {modelCardName = a} :: UpdateModelCard)

instance Core.AWSRequest UpdateModelCard where
  type
    AWSResponse UpdateModelCard =
      UpdateModelCardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateModelCardResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelCardArn")
      )

instance Prelude.Hashable UpdateModelCard where
  hashWithSalt _salt UpdateModelCard' {..} =
    _salt `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` modelCardStatus
      `Prelude.hashWithSalt` modelCardName

instance Prelude.NFData UpdateModelCard where
  rnf UpdateModelCard' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf modelCardStatus
      `Prelude.seq` Prelude.rnf modelCardName

instance Data.ToHeaders UpdateModelCard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateModelCard" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateModelCard where
  toJSON UpdateModelCard' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Content" Data..=) Prelude.<$> content,
            ("ModelCardStatus" Data..=)
              Prelude.<$> modelCardStatus,
            Prelude.Just
              ("ModelCardName" Data..= modelCardName)
          ]
      )

instance Data.ToPath UpdateModelCard where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateModelCard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateModelCardResponse' smart constructor.
data UpdateModelCardResponse = UpdateModelCardResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated model card.
    modelCardArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModelCardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateModelCardResponse_httpStatus' - The response's http status code.
--
-- 'modelCardArn', 'updateModelCardResponse_modelCardArn' - The Amazon Resource Name (ARN) of the updated model card.
newUpdateModelCardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelCardArn'
  Prelude.Text ->
  UpdateModelCardResponse
newUpdateModelCardResponse
  pHttpStatus_
  pModelCardArn_ =
    UpdateModelCardResponse'
      { httpStatus = pHttpStatus_,
        modelCardArn = pModelCardArn_
      }

-- | The response's http status code.
updateModelCardResponse_httpStatus :: Lens.Lens' UpdateModelCardResponse Prelude.Int
updateModelCardResponse_httpStatus = Lens.lens (\UpdateModelCardResponse' {httpStatus} -> httpStatus) (\s@UpdateModelCardResponse' {} a -> s {httpStatus = a} :: UpdateModelCardResponse)

-- | The Amazon Resource Name (ARN) of the updated model card.
updateModelCardResponse_modelCardArn :: Lens.Lens' UpdateModelCardResponse Prelude.Text
updateModelCardResponse_modelCardArn = Lens.lens (\UpdateModelCardResponse' {modelCardArn} -> modelCardArn) (\s@UpdateModelCardResponse' {} a -> s {modelCardArn = a} :: UpdateModelCardResponse)

instance Prelude.NFData UpdateModelCardResponse where
  rnf UpdateModelCardResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardArn
