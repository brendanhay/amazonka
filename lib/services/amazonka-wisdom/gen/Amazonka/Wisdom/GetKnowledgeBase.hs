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
-- Module      : Amazonka.Wisdom.GetKnowledgeBase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the knowledge base.
module Amazonka.Wisdom.GetKnowledgeBase
  ( -- * Creating a Request
    GetKnowledgeBase (..),
    newGetKnowledgeBase,

    -- * Request Lenses
    getKnowledgeBase_knowledgeBaseId,

    -- * Destructuring the Response
    GetKnowledgeBaseResponse (..),
    newGetKnowledgeBaseResponse,

    -- * Response Lenses
    getKnowledgeBaseResponse_knowledgeBase,
    getKnowledgeBaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetKnowledgeBase' smart constructor.
data GetKnowledgeBase = GetKnowledgeBase'
  { -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKnowledgeBase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBaseId', 'getKnowledgeBase_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newGetKnowledgeBase ::
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  GetKnowledgeBase
newGetKnowledgeBase pKnowledgeBaseId_ =
  GetKnowledgeBase'
    { knowledgeBaseId =
        pKnowledgeBaseId_
    }

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getKnowledgeBase_knowledgeBaseId :: Lens.Lens' GetKnowledgeBase Prelude.Text
getKnowledgeBase_knowledgeBaseId = Lens.lens (\GetKnowledgeBase' {knowledgeBaseId} -> knowledgeBaseId) (\s@GetKnowledgeBase' {} a -> s {knowledgeBaseId = a} :: GetKnowledgeBase)

instance Core.AWSRequest GetKnowledgeBase where
  type
    AWSResponse GetKnowledgeBase =
      GetKnowledgeBaseResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKnowledgeBaseResponse'
            Prelude.<$> (x Data..?> "knowledgeBase")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKnowledgeBase where
  hashWithSalt _salt GetKnowledgeBase' {..} =
    _salt `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData GetKnowledgeBase where
  rnf GetKnowledgeBase' {..} =
    Prelude.rnf knowledgeBaseId

instance Data.ToHeaders GetKnowledgeBase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKnowledgeBase where
  toPath GetKnowledgeBase' {..} =
    Prelude.mconcat
      ["/knowledgeBases/", Data.toBS knowledgeBaseId]

instance Data.ToQuery GetKnowledgeBase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKnowledgeBaseResponse' smart constructor.
data GetKnowledgeBaseResponse = GetKnowledgeBaseResponse'
  { -- | The knowledge base.
    knowledgeBase :: Prelude.Maybe KnowledgeBaseData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKnowledgeBaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'knowledgeBase', 'getKnowledgeBaseResponse_knowledgeBase' - The knowledge base.
--
-- 'httpStatus', 'getKnowledgeBaseResponse_httpStatus' - The response's http status code.
newGetKnowledgeBaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKnowledgeBaseResponse
newGetKnowledgeBaseResponse pHttpStatus_ =
  GetKnowledgeBaseResponse'
    { knowledgeBase =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The knowledge base.
getKnowledgeBaseResponse_knowledgeBase :: Lens.Lens' GetKnowledgeBaseResponse (Prelude.Maybe KnowledgeBaseData)
getKnowledgeBaseResponse_knowledgeBase = Lens.lens (\GetKnowledgeBaseResponse' {knowledgeBase} -> knowledgeBase) (\s@GetKnowledgeBaseResponse' {} a -> s {knowledgeBase = a} :: GetKnowledgeBaseResponse)

-- | The response's http status code.
getKnowledgeBaseResponse_httpStatus :: Lens.Lens' GetKnowledgeBaseResponse Prelude.Int
getKnowledgeBaseResponse_httpStatus = Lens.lens (\GetKnowledgeBaseResponse' {httpStatus} -> httpStatus) (\s@GetKnowledgeBaseResponse' {} a -> s {httpStatus = a} :: GetKnowledgeBaseResponse)

instance Prelude.NFData GetKnowledgeBaseResponse where
  rnf GetKnowledgeBaseResponse' {..} =
    Prelude.rnf knowledgeBase `Prelude.seq`
      Prelude.rnf httpStatus
