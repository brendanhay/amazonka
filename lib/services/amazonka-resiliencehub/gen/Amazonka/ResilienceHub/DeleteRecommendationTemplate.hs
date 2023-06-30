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
-- Module      : Amazonka.ResilienceHub.DeleteRecommendationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a recommendation template. This is a destructive action that
-- can\'t be undone.
module Amazonka.ResilienceHub.DeleteRecommendationTemplate
  ( -- * Creating a Request
    DeleteRecommendationTemplate (..),
    newDeleteRecommendationTemplate,

    -- * Request Lenses
    deleteRecommendationTemplate_clientToken,
    deleteRecommendationTemplate_recommendationTemplateArn,

    -- * Destructuring the Response
    DeleteRecommendationTemplateResponse (..),
    newDeleteRecommendationTemplateResponse,

    -- * Response Lenses
    deleteRecommendationTemplateResponse_httpStatus,
    deleteRecommendationTemplateResponse_recommendationTemplateArn,
    deleteRecommendationTemplateResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecommendationTemplate' smart constructor.
data DeleteRecommendationTemplate = DeleteRecommendationTemplate'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for a recommendation template.
    recommendationTemplateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommendationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteRecommendationTemplate_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'recommendationTemplateArn', 'deleteRecommendationTemplate_recommendationTemplateArn' - The Amazon Resource Name (ARN) for a recommendation template.
newDeleteRecommendationTemplate ::
  -- | 'recommendationTemplateArn'
  Prelude.Text ->
  DeleteRecommendationTemplate
newDeleteRecommendationTemplate
  pRecommendationTemplateArn_ =
    DeleteRecommendationTemplate'
      { clientToken =
          Prelude.Nothing,
        recommendationTemplateArn =
          pRecommendationTemplateArn_
      }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
deleteRecommendationTemplate_clientToken :: Lens.Lens' DeleteRecommendationTemplate (Prelude.Maybe Prelude.Text)
deleteRecommendationTemplate_clientToken = Lens.lens (\DeleteRecommendationTemplate' {clientToken} -> clientToken) (\s@DeleteRecommendationTemplate' {} a -> s {clientToken = a} :: DeleteRecommendationTemplate)

-- | The Amazon Resource Name (ARN) for a recommendation template.
deleteRecommendationTemplate_recommendationTemplateArn :: Lens.Lens' DeleteRecommendationTemplate Prelude.Text
deleteRecommendationTemplate_recommendationTemplateArn = Lens.lens (\DeleteRecommendationTemplate' {recommendationTemplateArn} -> recommendationTemplateArn) (\s@DeleteRecommendationTemplate' {} a -> s {recommendationTemplateArn = a} :: DeleteRecommendationTemplate)

instance Core.AWSRequest DeleteRecommendationTemplate where
  type
    AWSResponse DeleteRecommendationTemplate =
      DeleteRecommendationTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRecommendationTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "recommendationTemplateArn")
            Prelude.<*> (x Data..:> "status")
      )

instance
  Prelude.Hashable
    DeleteRecommendationTemplate
  where
  hashWithSalt _salt DeleteRecommendationTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` recommendationTemplateArn

instance Prelude.NFData DeleteRecommendationTemplate where
  rnf DeleteRecommendationTemplate' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf recommendationTemplateArn

instance Data.ToHeaders DeleteRecommendationTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRecommendationTemplate where
  toJSON DeleteRecommendationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "recommendationTemplateArn"
                  Data..= recommendationTemplateArn
              )
          ]
      )

instance Data.ToPath DeleteRecommendationTemplate where
  toPath =
    Prelude.const "/delete-recommendation-template"

instance Data.ToQuery DeleteRecommendationTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecommendationTemplateResponse' smart constructor.
data DeleteRecommendationTemplateResponse = DeleteRecommendationTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) for a recommendation template.
    recommendationTemplateArn :: Prelude.Text,
    -- | The status of the action.
    status :: RecommendationTemplateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommendationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRecommendationTemplateResponse_httpStatus' - The response's http status code.
--
-- 'recommendationTemplateArn', 'deleteRecommendationTemplateResponse_recommendationTemplateArn' - The Amazon Resource Name (ARN) for a recommendation template.
--
-- 'status', 'deleteRecommendationTemplateResponse_status' - The status of the action.
newDeleteRecommendationTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recommendationTemplateArn'
  Prelude.Text ->
  -- | 'status'
  RecommendationTemplateStatus ->
  DeleteRecommendationTemplateResponse
newDeleteRecommendationTemplateResponse
  pHttpStatus_
  pRecommendationTemplateArn_
  pStatus_ =
    DeleteRecommendationTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        recommendationTemplateArn =
          pRecommendationTemplateArn_,
        status = pStatus_
      }

-- | The response's http status code.
deleteRecommendationTemplateResponse_httpStatus :: Lens.Lens' DeleteRecommendationTemplateResponse Prelude.Int
deleteRecommendationTemplateResponse_httpStatus = Lens.lens (\DeleteRecommendationTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteRecommendationTemplateResponse' {} a -> s {httpStatus = a} :: DeleteRecommendationTemplateResponse)

-- | The Amazon Resource Name (ARN) for a recommendation template.
deleteRecommendationTemplateResponse_recommendationTemplateArn :: Lens.Lens' DeleteRecommendationTemplateResponse Prelude.Text
deleteRecommendationTemplateResponse_recommendationTemplateArn = Lens.lens (\DeleteRecommendationTemplateResponse' {recommendationTemplateArn} -> recommendationTemplateArn) (\s@DeleteRecommendationTemplateResponse' {} a -> s {recommendationTemplateArn = a} :: DeleteRecommendationTemplateResponse)

-- | The status of the action.
deleteRecommendationTemplateResponse_status :: Lens.Lens' DeleteRecommendationTemplateResponse RecommendationTemplateStatus
deleteRecommendationTemplateResponse_status = Lens.lens (\DeleteRecommendationTemplateResponse' {status} -> status) (\s@DeleteRecommendationTemplateResponse' {} a -> s {status = a} :: DeleteRecommendationTemplateResponse)

instance
  Prelude.NFData
    DeleteRecommendationTemplateResponse
  where
  rnf DeleteRecommendationTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recommendationTemplateArn
      `Prelude.seq` Prelude.rnf status
