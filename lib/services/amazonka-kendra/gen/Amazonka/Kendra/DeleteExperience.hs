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
-- Module      : Amazonka.Kendra.DeleteExperience
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Kendra experience such as a search application. For
-- more information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.DeleteExperience
  ( -- * Creating a Request
    DeleteExperience (..),
    newDeleteExperience,

    -- * Request Lenses
    deleteExperience_id,
    deleteExperience_indexId,

    -- * Destructuring the Response
    DeleteExperienceResponse (..),
    newDeleteExperienceResponse,

    -- * Response Lenses
    deleteExperienceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExperience' smart constructor.
data DeleteExperience = DeleteExperience'
  { -- | The identifier of your Amazon Kendra experience you want to delete.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExperience' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteExperience_id' - The identifier of your Amazon Kendra experience you want to delete.
--
-- 'indexId', 'deleteExperience_indexId' - The identifier of the index for your Amazon Kendra experience.
newDeleteExperience ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DeleteExperience
newDeleteExperience pId_ pIndexId_ =
  DeleteExperience' {id = pId_, indexId = pIndexId_}

-- | The identifier of your Amazon Kendra experience you want to delete.
deleteExperience_id :: Lens.Lens' DeleteExperience Prelude.Text
deleteExperience_id = Lens.lens (\DeleteExperience' {id} -> id) (\s@DeleteExperience' {} a -> s {id = a} :: DeleteExperience)

-- | The identifier of the index for your Amazon Kendra experience.
deleteExperience_indexId :: Lens.Lens' DeleteExperience Prelude.Text
deleteExperience_indexId = Lens.lens (\DeleteExperience' {indexId} -> indexId) (\s@DeleteExperience' {} a -> s {indexId = a} :: DeleteExperience)

instance Core.AWSRequest DeleteExperience where
  type
    AWSResponse DeleteExperience =
      DeleteExperienceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteExperienceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteExperience where
  hashWithSalt _salt DeleteExperience' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData DeleteExperience where
  rnf DeleteExperience' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders DeleteExperience where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DeleteExperience" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteExperience where
  toJSON DeleteExperience' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath DeleteExperience where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteExperience where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExperienceResponse' smart constructor.
data DeleteExperienceResponse = DeleteExperienceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExperienceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteExperienceResponse_httpStatus' - The response's http status code.
newDeleteExperienceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteExperienceResponse
newDeleteExperienceResponse pHttpStatus_ =
  DeleteExperienceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteExperienceResponse_httpStatus :: Lens.Lens' DeleteExperienceResponse Prelude.Int
deleteExperienceResponse_httpStatus = Lens.lens (\DeleteExperienceResponse' {httpStatus} -> httpStatus) (\s@DeleteExperienceResponse' {} a -> s {httpStatus = a} :: DeleteExperienceResponse)

instance Prelude.NFData DeleteExperienceResponse where
  rnf DeleteExperienceResponse' {..} =
    Prelude.rnf httpStatus
