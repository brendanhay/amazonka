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
-- Module      : Amazonka.Kendra.AssociateEntitiesToExperience
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants users or groups in your IAM Identity Center identity source
-- access to your Amazon Kendra experience. You can create an Amazon Kendra
-- experience such as a search application. For more information on
-- creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.AssociateEntitiesToExperience
  ( -- * Creating a Request
    AssociateEntitiesToExperience (..),
    newAssociateEntitiesToExperience,

    -- * Request Lenses
    associateEntitiesToExperience_id,
    associateEntitiesToExperience_indexId,
    associateEntitiesToExperience_entityList,

    -- * Destructuring the Response
    AssociateEntitiesToExperienceResponse (..),
    newAssociateEntitiesToExperienceResponse,

    -- * Response Lenses
    associateEntitiesToExperienceResponse_failedEntityList,
    associateEntitiesToExperienceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateEntitiesToExperience' smart constructor.
data AssociateEntitiesToExperience = AssociateEntitiesToExperience'
  { -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text,
    -- | Lists users or groups in your IAM Identity Center identity source.
    entityList :: Prelude.NonEmpty EntityConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateEntitiesToExperience' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateEntitiesToExperience_id' - The identifier of your Amazon Kendra experience.
--
-- 'indexId', 'associateEntitiesToExperience_indexId' - The identifier of the index for your Amazon Kendra experience.
--
-- 'entityList', 'associateEntitiesToExperience_entityList' - Lists users or groups in your IAM Identity Center identity source.
newAssociateEntitiesToExperience ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  -- | 'entityList'
  Prelude.NonEmpty EntityConfiguration ->
  AssociateEntitiesToExperience
newAssociateEntitiesToExperience
  pId_
  pIndexId_
  pEntityList_ =
    AssociateEntitiesToExperience'
      { id = pId_,
        indexId = pIndexId_,
        entityList =
          Lens.coerced Lens.# pEntityList_
      }

-- | The identifier of your Amazon Kendra experience.
associateEntitiesToExperience_id :: Lens.Lens' AssociateEntitiesToExperience Prelude.Text
associateEntitiesToExperience_id = Lens.lens (\AssociateEntitiesToExperience' {id} -> id) (\s@AssociateEntitiesToExperience' {} a -> s {id = a} :: AssociateEntitiesToExperience)

-- | The identifier of the index for your Amazon Kendra experience.
associateEntitiesToExperience_indexId :: Lens.Lens' AssociateEntitiesToExperience Prelude.Text
associateEntitiesToExperience_indexId = Lens.lens (\AssociateEntitiesToExperience' {indexId} -> indexId) (\s@AssociateEntitiesToExperience' {} a -> s {indexId = a} :: AssociateEntitiesToExperience)

-- | Lists users or groups in your IAM Identity Center identity source.
associateEntitiesToExperience_entityList :: Lens.Lens' AssociateEntitiesToExperience (Prelude.NonEmpty EntityConfiguration)
associateEntitiesToExperience_entityList = Lens.lens (\AssociateEntitiesToExperience' {entityList} -> entityList) (\s@AssociateEntitiesToExperience' {} a -> s {entityList = a} :: AssociateEntitiesToExperience) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AssociateEntitiesToExperience
  where
  type
    AWSResponse AssociateEntitiesToExperience =
      AssociateEntitiesToExperienceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateEntitiesToExperienceResponse'
            Prelude.<$> (x Core..?> "FailedEntityList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateEntitiesToExperience
  where
  hashWithSalt _salt AssociateEntitiesToExperience' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` entityList

instance Prelude.NFData AssociateEntitiesToExperience where
  rnf AssociateEntitiesToExperience' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf entityList

instance Core.ToHeaders AssociateEntitiesToExperience where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.AssociateEntitiesToExperience" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateEntitiesToExperience where
  toJSON AssociateEntitiesToExperience' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("EntityList" Core..= entityList)
          ]
      )

instance Core.ToPath AssociateEntitiesToExperience where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateEntitiesToExperience where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateEntitiesToExperienceResponse' smart constructor.
data AssociateEntitiesToExperienceResponse = AssociateEntitiesToExperienceResponse'
  { -- | Lists the users or groups in your IAM Identity Center identity source
    -- that failed to properly configure with your Amazon Kendra experience.
    failedEntityList :: Prelude.Maybe (Prelude.NonEmpty FailedEntity),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateEntitiesToExperienceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntityList', 'associateEntitiesToExperienceResponse_failedEntityList' - Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly configure with your Amazon Kendra experience.
--
-- 'httpStatus', 'associateEntitiesToExperienceResponse_httpStatus' - The response's http status code.
newAssociateEntitiesToExperienceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateEntitiesToExperienceResponse
newAssociateEntitiesToExperienceResponse pHttpStatus_ =
  AssociateEntitiesToExperienceResponse'
    { failedEntityList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly configure with your Amazon Kendra experience.
associateEntitiesToExperienceResponse_failedEntityList :: Lens.Lens' AssociateEntitiesToExperienceResponse (Prelude.Maybe (Prelude.NonEmpty FailedEntity))
associateEntitiesToExperienceResponse_failedEntityList = Lens.lens (\AssociateEntitiesToExperienceResponse' {failedEntityList} -> failedEntityList) (\s@AssociateEntitiesToExperienceResponse' {} a -> s {failedEntityList = a} :: AssociateEntitiesToExperienceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associateEntitiesToExperienceResponse_httpStatus :: Lens.Lens' AssociateEntitiesToExperienceResponse Prelude.Int
associateEntitiesToExperienceResponse_httpStatus = Lens.lens (\AssociateEntitiesToExperienceResponse' {httpStatus} -> httpStatus) (\s@AssociateEntitiesToExperienceResponse' {} a -> s {httpStatus = a} :: AssociateEntitiesToExperienceResponse)

instance
  Prelude.NFData
    AssociateEntitiesToExperienceResponse
  where
  rnf AssociateEntitiesToExperienceResponse' {..} =
    Prelude.rnf failedEntityList
      `Prelude.seq` Prelude.rnf httpStatus
