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
-- Module      : Amazonka.Kendra.DisassociateEntitiesFromExperience
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Prevents users or groups in your IAM Identity Center identity source
-- from accessing your Amazon Kendra experience. You can create an Amazon
-- Kendra experience such as a search application. For more information on
-- creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.DisassociateEntitiesFromExperience
  ( -- * Creating a Request
    DisassociateEntitiesFromExperience (..),
    newDisassociateEntitiesFromExperience,

    -- * Request Lenses
    disassociateEntitiesFromExperience_id,
    disassociateEntitiesFromExperience_indexId,
    disassociateEntitiesFromExperience_entityList,

    -- * Destructuring the Response
    DisassociateEntitiesFromExperienceResponse (..),
    newDisassociateEntitiesFromExperienceResponse,

    -- * Response Lenses
    disassociateEntitiesFromExperienceResponse_failedEntityList,
    disassociateEntitiesFromExperienceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateEntitiesFromExperience' smart constructor.
data DisassociateEntitiesFromExperience = DisassociateEntitiesFromExperience'
  { -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text,
    -- | Lists users or groups in your IAM Identity Center identity source.
    entityList :: Prelude.NonEmpty EntityConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateEntitiesFromExperience' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateEntitiesFromExperience_id' - The identifier of your Amazon Kendra experience.
--
-- 'indexId', 'disassociateEntitiesFromExperience_indexId' - The identifier of the index for your Amazon Kendra experience.
--
-- 'entityList', 'disassociateEntitiesFromExperience_entityList' - Lists users or groups in your IAM Identity Center identity source.
newDisassociateEntitiesFromExperience ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  -- | 'entityList'
  Prelude.NonEmpty EntityConfiguration ->
  DisassociateEntitiesFromExperience
newDisassociateEntitiesFromExperience
  pId_
  pIndexId_
  pEntityList_ =
    DisassociateEntitiesFromExperience'
      { id = pId_,
        indexId = pIndexId_,
        entityList =
          Lens.coerced Lens.# pEntityList_
      }

-- | The identifier of your Amazon Kendra experience.
disassociateEntitiesFromExperience_id :: Lens.Lens' DisassociateEntitiesFromExperience Prelude.Text
disassociateEntitiesFromExperience_id = Lens.lens (\DisassociateEntitiesFromExperience' {id} -> id) (\s@DisassociateEntitiesFromExperience' {} a -> s {id = a} :: DisassociateEntitiesFromExperience)

-- | The identifier of the index for your Amazon Kendra experience.
disassociateEntitiesFromExperience_indexId :: Lens.Lens' DisassociateEntitiesFromExperience Prelude.Text
disassociateEntitiesFromExperience_indexId = Lens.lens (\DisassociateEntitiesFromExperience' {indexId} -> indexId) (\s@DisassociateEntitiesFromExperience' {} a -> s {indexId = a} :: DisassociateEntitiesFromExperience)

-- | Lists users or groups in your IAM Identity Center identity source.
disassociateEntitiesFromExperience_entityList :: Lens.Lens' DisassociateEntitiesFromExperience (Prelude.NonEmpty EntityConfiguration)
disassociateEntitiesFromExperience_entityList = Lens.lens (\DisassociateEntitiesFromExperience' {entityList} -> entityList) (\s@DisassociateEntitiesFromExperience' {} a -> s {entityList = a} :: DisassociateEntitiesFromExperience) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DisassociateEntitiesFromExperience
  where
  type
    AWSResponse DisassociateEntitiesFromExperience =
      DisassociateEntitiesFromExperienceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateEntitiesFromExperienceResponse'
            Prelude.<$> (x Data..?> "FailedEntityList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateEntitiesFromExperience
  where
  hashWithSalt
    _salt
    DisassociateEntitiesFromExperience' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` entityList

instance
  Prelude.NFData
    DisassociateEntitiesFromExperience
  where
  rnf DisassociateEntitiesFromExperience' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf entityList

instance
  Data.ToHeaders
    DisassociateEntitiesFromExperience
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DisassociateEntitiesFromExperience" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateEntitiesFromExperience
  where
  toJSON DisassociateEntitiesFromExperience' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("EntityList" Data..= entityList)
          ]
      )

instance
  Data.ToPath
    DisassociateEntitiesFromExperience
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateEntitiesFromExperience
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateEntitiesFromExperienceResponse' smart constructor.
data DisassociateEntitiesFromExperienceResponse = DisassociateEntitiesFromExperienceResponse'
  { -- | Lists the users or groups in your IAM Identity Center identity source
    -- that failed to properly remove access to your Amazon Kendra experience.
    failedEntityList :: Prelude.Maybe (Prelude.NonEmpty FailedEntity),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateEntitiesFromExperienceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntityList', 'disassociateEntitiesFromExperienceResponse_failedEntityList' - Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly remove access to your Amazon Kendra experience.
--
-- 'httpStatus', 'disassociateEntitiesFromExperienceResponse_httpStatus' - The response's http status code.
newDisassociateEntitiesFromExperienceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateEntitiesFromExperienceResponse
newDisassociateEntitiesFromExperienceResponse
  pHttpStatus_ =
    DisassociateEntitiesFromExperienceResponse'
      { failedEntityList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly remove access to your Amazon Kendra experience.
disassociateEntitiesFromExperienceResponse_failedEntityList :: Lens.Lens' DisassociateEntitiesFromExperienceResponse (Prelude.Maybe (Prelude.NonEmpty FailedEntity))
disassociateEntitiesFromExperienceResponse_failedEntityList = Lens.lens (\DisassociateEntitiesFromExperienceResponse' {failedEntityList} -> failedEntityList) (\s@DisassociateEntitiesFromExperienceResponse' {} a -> s {failedEntityList = a} :: DisassociateEntitiesFromExperienceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociateEntitiesFromExperienceResponse_httpStatus :: Lens.Lens' DisassociateEntitiesFromExperienceResponse Prelude.Int
disassociateEntitiesFromExperienceResponse_httpStatus = Lens.lens (\DisassociateEntitiesFromExperienceResponse' {httpStatus} -> httpStatus) (\s@DisassociateEntitiesFromExperienceResponse' {} a -> s {httpStatus = a} :: DisassociateEntitiesFromExperienceResponse)

instance
  Prelude.NFData
    DisassociateEntitiesFromExperienceResponse
  where
  rnf DisassociateEntitiesFromExperienceResponse' {..} =
    Prelude.rnf failedEntityList
      `Prelude.seq` Prelude.rnf httpStatus
