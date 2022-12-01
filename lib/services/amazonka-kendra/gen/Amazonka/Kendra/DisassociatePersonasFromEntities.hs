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
-- Module      : Amazonka.Kendra.DisassociatePersonasFromEntities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specific permissions of users or groups in your IAM Identity
-- Center identity source with access to your Amazon Kendra experience. You
-- can create an Amazon Kendra experience such as a search application. For
-- more information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.DisassociatePersonasFromEntities
  ( -- * Creating a Request
    DisassociatePersonasFromEntities (..),
    newDisassociatePersonasFromEntities,

    -- * Request Lenses
    disassociatePersonasFromEntities_id,
    disassociatePersonasFromEntities_indexId,
    disassociatePersonasFromEntities_entityIds,

    -- * Destructuring the Response
    DisassociatePersonasFromEntitiesResponse (..),
    newDisassociatePersonasFromEntitiesResponse,

    -- * Response Lenses
    disassociatePersonasFromEntitiesResponse_failedEntityList,
    disassociatePersonasFromEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociatePersonasFromEntities' smart constructor.
data DisassociatePersonasFromEntities = DisassociatePersonasFromEntities'
  { -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text,
    -- | The identifiers of users or groups in your IAM Identity Center identity
    -- source. For example, user IDs could be user emails.
    entityIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePersonasFromEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociatePersonasFromEntities_id' - The identifier of your Amazon Kendra experience.
--
-- 'indexId', 'disassociatePersonasFromEntities_indexId' - The identifier of the index for your Amazon Kendra experience.
--
-- 'entityIds', 'disassociatePersonasFromEntities_entityIds' - The identifiers of users or groups in your IAM Identity Center identity
-- source. For example, user IDs could be user emails.
newDisassociatePersonasFromEntities ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  -- | 'entityIds'
  Prelude.NonEmpty Prelude.Text ->
  DisassociatePersonasFromEntities
newDisassociatePersonasFromEntities
  pId_
  pIndexId_
  pEntityIds_ =
    DisassociatePersonasFromEntities'
      { id = pId_,
        indexId = pIndexId_,
        entityIds =
          Lens.coerced Lens.# pEntityIds_
      }

-- | The identifier of your Amazon Kendra experience.
disassociatePersonasFromEntities_id :: Lens.Lens' DisassociatePersonasFromEntities Prelude.Text
disassociatePersonasFromEntities_id = Lens.lens (\DisassociatePersonasFromEntities' {id} -> id) (\s@DisassociatePersonasFromEntities' {} a -> s {id = a} :: DisassociatePersonasFromEntities)

-- | The identifier of the index for your Amazon Kendra experience.
disassociatePersonasFromEntities_indexId :: Lens.Lens' DisassociatePersonasFromEntities Prelude.Text
disassociatePersonasFromEntities_indexId = Lens.lens (\DisassociatePersonasFromEntities' {indexId} -> indexId) (\s@DisassociatePersonasFromEntities' {} a -> s {indexId = a} :: DisassociatePersonasFromEntities)

-- | The identifiers of users or groups in your IAM Identity Center identity
-- source. For example, user IDs could be user emails.
disassociatePersonasFromEntities_entityIds :: Lens.Lens' DisassociatePersonasFromEntities (Prelude.NonEmpty Prelude.Text)
disassociatePersonasFromEntities_entityIds = Lens.lens (\DisassociatePersonasFromEntities' {entityIds} -> entityIds) (\s@DisassociatePersonasFromEntities' {} a -> s {entityIds = a} :: DisassociatePersonasFromEntities) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociatePersonasFromEntities
  where
  type
    AWSResponse DisassociatePersonasFromEntities =
      DisassociatePersonasFromEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociatePersonasFromEntitiesResponse'
            Prelude.<$> (x Core..?> "FailedEntityList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociatePersonasFromEntities
  where
  hashWithSalt
    _salt
    DisassociatePersonasFromEntities' {..} =
      _salt `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` entityIds

instance
  Prelude.NFData
    DisassociatePersonasFromEntities
  where
  rnf DisassociatePersonasFromEntities' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf entityIds

instance
  Core.ToHeaders
    DisassociatePersonasFromEntities
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DisassociatePersonasFromEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociatePersonasFromEntities where
  toJSON DisassociatePersonasFromEntities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("EntityIds" Core..= entityIds)
          ]
      )

instance Core.ToPath DisassociatePersonasFromEntities where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DisassociatePersonasFromEntities
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociatePersonasFromEntitiesResponse' smart constructor.
data DisassociatePersonasFromEntitiesResponse = DisassociatePersonasFromEntitiesResponse'
  { -- | Lists the users or groups in your IAM Identity Center identity source
    -- that failed to properly remove access to your Amazon Kendra experience.
    failedEntityList :: Prelude.Maybe (Prelude.NonEmpty FailedEntity),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePersonasFromEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntityList', 'disassociatePersonasFromEntitiesResponse_failedEntityList' - Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly remove access to your Amazon Kendra experience.
--
-- 'httpStatus', 'disassociatePersonasFromEntitiesResponse_httpStatus' - The response's http status code.
newDisassociatePersonasFromEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociatePersonasFromEntitiesResponse
newDisassociatePersonasFromEntitiesResponse
  pHttpStatus_ =
    DisassociatePersonasFromEntitiesResponse'
      { failedEntityList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly remove access to your Amazon Kendra experience.
disassociatePersonasFromEntitiesResponse_failedEntityList :: Lens.Lens' DisassociatePersonasFromEntitiesResponse (Prelude.Maybe (Prelude.NonEmpty FailedEntity))
disassociatePersonasFromEntitiesResponse_failedEntityList = Lens.lens (\DisassociatePersonasFromEntitiesResponse' {failedEntityList} -> failedEntityList) (\s@DisassociatePersonasFromEntitiesResponse' {} a -> s {failedEntityList = a} :: DisassociatePersonasFromEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociatePersonasFromEntitiesResponse_httpStatus :: Lens.Lens' DisassociatePersonasFromEntitiesResponse Prelude.Int
disassociatePersonasFromEntitiesResponse_httpStatus = Lens.lens (\DisassociatePersonasFromEntitiesResponse' {httpStatus} -> httpStatus) (\s@DisassociatePersonasFromEntitiesResponse' {} a -> s {httpStatus = a} :: DisassociatePersonasFromEntitiesResponse)

instance
  Prelude.NFData
    DisassociatePersonasFromEntitiesResponse
  where
  rnf DisassociatePersonasFromEntitiesResponse' {..} =
    Prelude.rnf failedEntityList
      `Prelude.seq` Prelude.rnf httpStatus
