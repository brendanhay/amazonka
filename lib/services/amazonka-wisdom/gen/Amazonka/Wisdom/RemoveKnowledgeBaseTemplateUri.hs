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
-- Module      : Amazonka.Wisdom.RemoveKnowledgeBaseTemplateUri
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a URI template from a knowledge base.
module Amazonka.Wisdom.RemoveKnowledgeBaseTemplateUri
  ( -- * Creating a Request
    RemoveKnowledgeBaseTemplateUri (..),
    newRemoveKnowledgeBaseTemplateUri,

    -- * Request Lenses
    removeKnowledgeBaseTemplateUri_knowledgeBaseId,

    -- * Destructuring the Response
    RemoveKnowledgeBaseTemplateUriResponse (..),
    newRemoveKnowledgeBaseTemplateUriResponse,

    -- * Response Lenses
    removeKnowledgeBaseTemplateUriResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newRemoveKnowledgeBaseTemplateUri' smart constructor.
data RemoveKnowledgeBaseTemplateUri = RemoveKnowledgeBaseTemplateUri'
  { -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveKnowledgeBaseTemplateUri' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBaseId', 'removeKnowledgeBaseTemplateUri_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newRemoveKnowledgeBaseTemplateUri ::
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  RemoveKnowledgeBaseTemplateUri
newRemoveKnowledgeBaseTemplateUri pKnowledgeBaseId_ =
  RemoveKnowledgeBaseTemplateUri'
    { knowledgeBaseId =
        pKnowledgeBaseId_
    }

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
removeKnowledgeBaseTemplateUri_knowledgeBaseId :: Lens.Lens' RemoveKnowledgeBaseTemplateUri Prelude.Text
removeKnowledgeBaseTemplateUri_knowledgeBaseId = Lens.lens (\RemoveKnowledgeBaseTemplateUri' {knowledgeBaseId} -> knowledgeBaseId) (\s@RemoveKnowledgeBaseTemplateUri' {} a -> s {knowledgeBaseId = a} :: RemoveKnowledgeBaseTemplateUri)

instance
  Core.AWSRequest
    RemoveKnowledgeBaseTemplateUri
  where
  type
    AWSResponse RemoveKnowledgeBaseTemplateUri =
      RemoveKnowledgeBaseTemplateUriResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveKnowledgeBaseTemplateUriResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RemoveKnowledgeBaseTemplateUri
  where
  hashWithSalt
    _salt
    RemoveKnowledgeBaseTemplateUri' {..} =
      _salt `Prelude.hashWithSalt` knowledgeBaseId

instance
  Prelude.NFData
    RemoveKnowledgeBaseTemplateUri
  where
  rnf RemoveKnowledgeBaseTemplateUri' {..} =
    Prelude.rnf knowledgeBaseId

instance
  Data.ToHeaders
    RemoveKnowledgeBaseTemplateUri
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

instance Data.ToPath RemoveKnowledgeBaseTemplateUri where
  toPath RemoveKnowledgeBaseTemplateUri' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/templateUri"
      ]

instance Data.ToQuery RemoveKnowledgeBaseTemplateUri where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveKnowledgeBaseTemplateUriResponse' smart constructor.
data RemoveKnowledgeBaseTemplateUriResponse = RemoveKnowledgeBaseTemplateUriResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveKnowledgeBaseTemplateUriResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeKnowledgeBaseTemplateUriResponse_httpStatus' - The response's http status code.
newRemoveKnowledgeBaseTemplateUriResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveKnowledgeBaseTemplateUriResponse
newRemoveKnowledgeBaseTemplateUriResponse
  pHttpStatus_ =
    RemoveKnowledgeBaseTemplateUriResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
removeKnowledgeBaseTemplateUriResponse_httpStatus :: Lens.Lens' RemoveKnowledgeBaseTemplateUriResponse Prelude.Int
removeKnowledgeBaseTemplateUriResponse_httpStatus = Lens.lens (\RemoveKnowledgeBaseTemplateUriResponse' {httpStatus} -> httpStatus) (\s@RemoveKnowledgeBaseTemplateUriResponse' {} a -> s {httpStatus = a} :: RemoveKnowledgeBaseTemplateUriResponse)

instance
  Prelude.NFData
    RemoveKnowledgeBaseTemplateUriResponse
  where
  rnf RemoveKnowledgeBaseTemplateUriResponse' {..} =
    Prelude.rnf httpStatus
