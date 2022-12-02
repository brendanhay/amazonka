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
-- Module      : Amazonka.Wisdom.UpdateKnowledgeBaseTemplateUri
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the template URI of a knowledge base. This is only supported for
-- knowledge bases of type EXTERNAL. Include a single variable in
-- @${variable}@ format; this interpolated by Wisdom using ingested
-- content. For example, if you ingest a Salesforce article, it has an @Id@
-- value, and you can set the template URI to
-- @https:\/\/myInstanceName.lightning.force.com\/lightning\/r\/Knowledge__kav\/*${Id}*\/view@.
module Amazonka.Wisdom.UpdateKnowledgeBaseTemplateUri
  ( -- * Creating a Request
    UpdateKnowledgeBaseTemplateUri (..),
    newUpdateKnowledgeBaseTemplateUri,

    -- * Request Lenses
    updateKnowledgeBaseTemplateUri_knowledgeBaseId,
    updateKnowledgeBaseTemplateUri_templateUri,

    -- * Destructuring the Response
    UpdateKnowledgeBaseTemplateUriResponse (..),
    newUpdateKnowledgeBaseTemplateUriResponse,

    -- * Response Lenses
    updateKnowledgeBaseTemplateUriResponse_knowledgeBase,
    updateKnowledgeBaseTemplateUriResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newUpdateKnowledgeBaseTemplateUri' smart constructor.
data UpdateKnowledgeBaseTemplateUri = UpdateKnowledgeBaseTemplateUri'
  { -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text,
    -- | The template URI to update.
    templateUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKnowledgeBaseTemplateUri' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBaseId', 'updateKnowledgeBaseTemplateUri_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'templateUri', 'updateKnowledgeBaseTemplateUri_templateUri' - The template URI to update.
newUpdateKnowledgeBaseTemplateUri ::
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  -- | 'templateUri'
  Prelude.Text ->
  UpdateKnowledgeBaseTemplateUri
newUpdateKnowledgeBaseTemplateUri
  pKnowledgeBaseId_
  pTemplateUri_ =
    UpdateKnowledgeBaseTemplateUri'
      { knowledgeBaseId =
          pKnowledgeBaseId_,
        templateUri = pTemplateUri_
      }

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
updateKnowledgeBaseTemplateUri_knowledgeBaseId :: Lens.Lens' UpdateKnowledgeBaseTemplateUri Prelude.Text
updateKnowledgeBaseTemplateUri_knowledgeBaseId = Lens.lens (\UpdateKnowledgeBaseTemplateUri' {knowledgeBaseId} -> knowledgeBaseId) (\s@UpdateKnowledgeBaseTemplateUri' {} a -> s {knowledgeBaseId = a} :: UpdateKnowledgeBaseTemplateUri)

-- | The template URI to update.
updateKnowledgeBaseTemplateUri_templateUri :: Lens.Lens' UpdateKnowledgeBaseTemplateUri Prelude.Text
updateKnowledgeBaseTemplateUri_templateUri = Lens.lens (\UpdateKnowledgeBaseTemplateUri' {templateUri} -> templateUri) (\s@UpdateKnowledgeBaseTemplateUri' {} a -> s {templateUri = a} :: UpdateKnowledgeBaseTemplateUri)

instance
  Core.AWSRequest
    UpdateKnowledgeBaseTemplateUri
  where
  type
    AWSResponse UpdateKnowledgeBaseTemplateUri =
      UpdateKnowledgeBaseTemplateUriResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateKnowledgeBaseTemplateUriResponse'
            Prelude.<$> (x Data..?> "knowledgeBase")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateKnowledgeBaseTemplateUri
  where
  hashWithSalt
    _salt
    UpdateKnowledgeBaseTemplateUri' {..} =
      _salt `Prelude.hashWithSalt` knowledgeBaseId
        `Prelude.hashWithSalt` templateUri

instance
  Prelude.NFData
    UpdateKnowledgeBaseTemplateUri
  where
  rnf UpdateKnowledgeBaseTemplateUri' {..} =
    Prelude.rnf knowledgeBaseId
      `Prelude.seq` Prelude.rnf templateUri

instance
  Data.ToHeaders
    UpdateKnowledgeBaseTemplateUri
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKnowledgeBaseTemplateUri where
  toJSON UpdateKnowledgeBaseTemplateUri' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("templateUri" Data..= templateUri)]
      )

instance Data.ToPath UpdateKnowledgeBaseTemplateUri where
  toPath UpdateKnowledgeBaseTemplateUri' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/templateUri"
      ]

instance Data.ToQuery UpdateKnowledgeBaseTemplateUri where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKnowledgeBaseTemplateUriResponse' smart constructor.
data UpdateKnowledgeBaseTemplateUriResponse = UpdateKnowledgeBaseTemplateUriResponse'
  { -- | The knowledge base to update.
    knowledgeBase :: Prelude.Maybe KnowledgeBaseData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKnowledgeBaseTemplateUriResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBase', 'updateKnowledgeBaseTemplateUriResponse_knowledgeBase' - The knowledge base to update.
--
-- 'httpStatus', 'updateKnowledgeBaseTemplateUriResponse_httpStatus' - The response's http status code.
newUpdateKnowledgeBaseTemplateUriResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKnowledgeBaseTemplateUriResponse
newUpdateKnowledgeBaseTemplateUriResponse
  pHttpStatus_ =
    UpdateKnowledgeBaseTemplateUriResponse'
      { knowledgeBase =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The knowledge base to update.
updateKnowledgeBaseTemplateUriResponse_knowledgeBase :: Lens.Lens' UpdateKnowledgeBaseTemplateUriResponse (Prelude.Maybe KnowledgeBaseData)
updateKnowledgeBaseTemplateUriResponse_knowledgeBase = Lens.lens (\UpdateKnowledgeBaseTemplateUriResponse' {knowledgeBase} -> knowledgeBase) (\s@UpdateKnowledgeBaseTemplateUriResponse' {} a -> s {knowledgeBase = a} :: UpdateKnowledgeBaseTemplateUriResponse)

-- | The response's http status code.
updateKnowledgeBaseTemplateUriResponse_httpStatus :: Lens.Lens' UpdateKnowledgeBaseTemplateUriResponse Prelude.Int
updateKnowledgeBaseTemplateUriResponse_httpStatus = Lens.lens (\UpdateKnowledgeBaseTemplateUriResponse' {httpStatus} -> httpStatus) (\s@UpdateKnowledgeBaseTemplateUriResponse' {} a -> s {httpStatus = a} :: UpdateKnowledgeBaseTemplateUriResponse)

instance
  Prelude.NFData
    UpdateKnowledgeBaseTemplateUriResponse
  where
  rnf UpdateKnowledgeBaseTemplateUriResponse' {..} =
    Prelude.rnf knowledgeBase
      `Prelude.seq` Prelude.rnf httpStatus
