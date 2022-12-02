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
-- Module      : Amazonka.ConnectCases.CreateRelatedItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a related item (comments, tasks, and contacts) and associates it
-- with a case.
--
-- A Related Item is a resource that is associated with a case. It may or
-- may not have an external identifier linking it to an external resource
-- (for example, a @contactArn@). All Related Items have their own internal
-- identifier, the @relatedItemArn@. Examples of related items include
-- @comments@ and @contacts@.
module Amazonka.ConnectCases.CreateRelatedItem
  ( -- * Creating a Request
    CreateRelatedItem (..),
    newCreateRelatedItem,

    -- * Request Lenses
    createRelatedItem_caseId,
    createRelatedItem_content,
    createRelatedItem_domainId,
    createRelatedItem_type,

    -- * Destructuring the Response
    CreateRelatedItemResponse (..),
    newCreateRelatedItemResponse,

    -- * Response Lenses
    createRelatedItemResponse_httpStatus,
    createRelatedItemResponse_relatedItemArn,
    createRelatedItemResponse_relatedItemId,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRelatedItem' smart constructor.
data CreateRelatedItem = CreateRelatedItem'
  { -- | A unique identifier of the case.
    caseId :: Prelude.Text,
    -- | The content of a related item to be created.
    content :: RelatedItemInputContent,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The type of a related item.
    type' :: RelatedItemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRelatedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseId', 'createRelatedItem_caseId' - A unique identifier of the case.
--
-- 'content', 'createRelatedItem_content' - The content of a related item to be created.
--
-- 'domainId', 'createRelatedItem_domainId' - The unique identifier of the Cases domain.
--
-- 'type'', 'createRelatedItem_type' - The type of a related item.
newCreateRelatedItem ::
  -- | 'caseId'
  Prelude.Text ->
  -- | 'content'
  RelatedItemInputContent ->
  -- | 'domainId'
  Prelude.Text ->
  -- | 'type''
  RelatedItemType ->
  CreateRelatedItem
newCreateRelatedItem
  pCaseId_
  pContent_
  pDomainId_
  pType_ =
    CreateRelatedItem'
      { caseId = pCaseId_,
        content = pContent_,
        domainId = pDomainId_,
        type' = pType_
      }

-- | A unique identifier of the case.
createRelatedItem_caseId :: Lens.Lens' CreateRelatedItem Prelude.Text
createRelatedItem_caseId = Lens.lens (\CreateRelatedItem' {caseId} -> caseId) (\s@CreateRelatedItem' {} a -> s {caseId = a} :: CreateRelatedItem)

-- | The content of a related item to be created.
createRelatedItem_content :: Lens.Lens' CreateRelatedItem RelatedItemInputContent
createRelatedItem_content = Lens.lens (\CreateRelatedItem' {content} -> content) (\s@CreateRelatedItem' {} a -> s {content = a} :: CreateRelatedItem)

-- | The unique identifier of the Cases domain.
createRelatedItem_domainId :: Lens.Lens' CreateRelatedItem Prelude.Text
createRelatedItem_domainId = Lens.lens (\CreateRelatedItem' {domainId} -> domainId) (\s@CreateRelatedItem' {} a -> s {domainId = a} :: CreateRelatedItem)

-- | The type of a related item.
createRelatedItem_type :: Lens.Lens' CreateRelatedItem RelatedItemType
createRelatedItem_type = Lens.lens (\CreateRelatedItem' {type'} -> type') (\s@CreateRelatedItem' {} a -> s {type' = a} :: CreateRelatedItem)

instance Core.AWSRequest CreateRelatedItem where
  type
    AWSResponse CreateRelatedItem =
      CreateRelatedItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelatedItemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "relatedItemArn")
            Prelude.<*> (x Data..:> "relatedItemId")
      )

instance Prelude.Hashable CreateRelatedItem where
  hashWithSalt _salt CreateRelatedItem' {..} =
    _salt `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateRelatedItem where
  rnf CreateRelatedItem' {..} =
    Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateRelatedItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRelatedItem where
  toJSON CreateRelatedItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("content" Data..= content),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateRelatedItem where
  toPath CreateRelatedItem' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/cases/",
        Data.toBS caseId,
        "/related-items/"
      ]

instance Data.ToQuery CreateRelatedItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRelatedItemResponse' smart constructor.
data CreateRelatedItemResponse = CreateRelatedItemResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the related item.
    relatedItemArn :: Prelude.Text,
    -- | The unique identifier of the related item.
    relatedItemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRelatedItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRelatedItemResponse_httpStatus' - The response's http status code.
--
-- 'relatedItemArn', 'createRelatedItemResponse_relatedItemArn' - The Amazon Resource Name (ARN) of the related item.
--
-- 'relatedItemId', 'createRelatedItemResponse_relatedItemId' - The unique identifier of the related item.
newCreateRelatedItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'relatedItemArn'
  Prelude.Text ->
  -- | 'relatedItemId'
  Prelude.Text ->
  CreateRelatedItemResponse
newCreateRelatedItemResponse
  pHttpStatus_
  pRelatedItemArn_
  pRelatedItemId_ =
    CreateRelatedItemResponse'
      { httpStatus =
          pHttpStatus_,
        relatedItemArn = pRelatedItemArn_,
        relatedItemId = pRelatedItemId_
      }

-- | The response's http status code.
createRelatedItemResponse_httpStatus :: Lens.Lens' CreateRelatedItemResponse Prelude.Int
createRelatedItemResponse_httpStatus = Lens.lens (\CreateRelatedItemResponse' {httpStatus} -> httpStatus) (\s@CreateRelatedItemResponse' {} a -> s {httpStatus = a} :: CreateRelatedItemResponse)

-- | The Amazon Resource Name (ARN) of the related item.
createRelatedItemResponse_relatedItemArn :: Lens.Lens' CreateRelatedItemResponse Prelude.Text
createRelatedItemResponse_relatedItemArn = Lens.lens (\CreateRelatedItemResponse' {relatedItemArn} -> relatedItemArn) (\s@CreateRelatedItemResponse' {} a -> s {relatedItemArn = a} :: CreateRelatedItemResponse)

-- | The unique identifier of the related item.
createRelatedItemResponse_relatedItemId :: Lens.Lens' CreateRelatedItemResponse Prelude.Text
createRelatedItemResponse_relatedItemId = Lens.lens (\CreateRelatedItemResponse' {relatedItemId} -> relatedItemId) (\s@CreateRelatedItemResponse' {} a -> s {relatedItemId = a} :: CreateRelatedItemResponse)

instance Prelude.NFData CreateRelatedItemResponse where
  rnf CreateRelatedItemResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf relatedItemArn
      `Prelude.seq` Prelude.rnf relatedItemId
