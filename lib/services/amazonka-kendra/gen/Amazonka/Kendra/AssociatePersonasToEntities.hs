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
-- Module      : Amazonka.Kendra.AssociatePersonasToEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the specific permissions of users or groups in your IAM Identity
-- Center identity source with access to your Amazon Kendra experience. You
-- can create an Amazon Kendra experience such as a search application. For
-- more information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.AssociatePersonasToEntities
  ( -- * Creating a Request
    AssociatePersonasToEntities (..),
    newAssociatePersonasToEntities,

    -- * Request Lenses
    associatePersonasToEntities_id,
    associatePersonasToEntities_indexId,
    associatePersonasToEntities_personas,

    -- * Destructuring the Response
    AssociatePersonasToEntitiesResponse (..),
    newAssociatePersonasToEntitiesResponse,

    -- * Response Lenses
    associatePersonasToEntitiesResponse_failedEntityList,
    associatePersonasToEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociatePersonasToEntities' smart constructor.
data AssociatePersonasToEntities = AssociatePersonasToEntities'
  { -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text,
    -- | The personas that define the specific permissions of users or groups in
    -- your IAM Identity Center identity source. The available personas or
    -- access roles are @Owner@ and @Viewer@. For more information on these
    -- personas, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html#access-search-experience Providing access to your search page>.
    personas :: Prelude.NonEmpty EntityPersonaConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePersonasToEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associatePersonasToEntities_id' - The identifier of your Amazon Kendra experience.
--
-- 'indexId', 'associatePersonasToEntities_indexId' - The identifier of the index for your Amazon Kendra experience.
--
-- 'personas', 'associatePersonasToEntities_personas' - The personas that define the specific permissions of users or groups in
-- your IAM Identity Center identity source. The available personas or
-- access roles are @Owner@ and @Viewer@. For more information on these
-- personas, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html#access-search-experience Providing access to your search page>.
newAssociatePersonasToEntities ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  -- | 'personas'
  Prelude.NonEmpty EntityPersonaConfiguration ->
  AssociatePersonasToEntities
newAssociatePersonasToEntities
  pId_
  pIndexId_
  pPersonas_ =
    AssociatePersonasToEntities'
      { id = pId_,
        indexId = pIndexId_,
        personas = Lens.coerced Lens.# pPersonas_
      }

-- | The identifier of your Amazon Kendra experience.
associatePersonasToEntities_id :: Lens.Lens' AssociatePersonasToEntities Prelude.Text
associatePersonasToEntities_id = Lens.lens (\AssociatePersonasToEntities' {id} -> id) (\s@AssociatePersonasToEntities' {} a -> s {id = a} :: AssociatePersonasToEntities)

-- | The identifier of the index for your Amazon Kendra experience.
associatePersonasToEntities_indexId :: Lens.Lens' AssociatePersonasToEntities Prelude.Text
associatePersonasToEntities_indexId = Lens.lens (\AssociatePersonasToEntities' {indexId} -> indexId) (\s@AssociatePersonasToEntities' {} a -> s {indexId = a} :: AssociatePersonasToEntities)

-- | The personas that define the specific permissions of users or groups in
-- your IAM Identity Center identity source. The available personas or
-- access roles are @Owner@ and @Viewer@. For more information on these
-- personas, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html#access-search-experience Providing access to your search page>.
associatePersonasToEntities_personas :: Lens.Lens' AssociatePersonasToEntities (Prelude.NonEmpty EntityPersonaConfiguration)
associatePersonasToEntities_personas = Lens.lens (\AssociatePersonasToEntities' {personas} -> personas) (\s@AssociatePersonasToEntities' {} a -> s {personas = a} :: AssociatePersonasToEntities) Prelude.. Lens.coerced

instance Core.AWSRequest AssociatePersonasToEntities where
  type
    AWSResponse AssociatePersonasToEntities =
      AssociatePersonasToEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociatePersonasToEntitiesResponse'
            Prelude.<$> (x Data..?> "FailedEntityList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociatePersonasToEntities where
  hashWithSalt _salt AssociatePersonasToEntities' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` personas

instance Prelude.NFData AssociatePersonasToEntities where
  rnf AssociatePersonasToEntities' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf personas

instance Data.ToHeaders AssociatePersonasToEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.AssociatePersonasToEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociatePersonasToEntities where
  toJSON AssociatePersonasToEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Personas" Data..= personas)
          ]
      )

instance Data.ToPath AssociatePersonasToEntities where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociatePersonasToEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociatePersonasToEntitiesResponse' smart constructor.
data AssociatePersonasToEntitiesResponse = AssociatePersonasToEntitiesResponse'
  { -- | Lists the users or groups in your IAM Identity Center identity source
    -- that failed to properly configure with your Amazon Kendra experience.
    failedEntityList :: Prelude.Maybe (Prelude.NonEmpty FailedEntity),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePersonasToEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntityList', 'associatePersonasToEntitiesResponse_failedEntityList' - Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly configure with your Amazon Kendra experience.
--
-- 'httpStatus', 'associatePersonasToEntitiesResponse_httpStatus' - The response's http status code.
newAssociatePersonasToEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePersonasToEntitiesResponse
newAssociatePersonasToEntitiesResponse pHttpStatus_ =
  AssociatePersonasToEntitiesResponse'
    { failedEntityList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the users or groups in your IAM Identity Center identity source
-- that failed to properly configure with your Amazon Kendra experience.
associatePersonasToEntitiesResponse_failedEntityList :: Lens.Lens' AssociatePersonasToEntitiesResponse (Prelude.Maybe (Prelude.NonEmpty FailedEntity))
associatePersonasToEntitiesResponse_failedEntityList = Lens.lens (\AssociatePersonasToEntitiesResponse' {failedEntityList} -> failedEntityList) (\s@AssociatePersonasToEntitiesResponse' {} a -> s {failedEntityList = a} :: AssociatePersonasToEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associatePersonasToEntitiesResponse_httpStatus :: Lens.Lens' AssociatePersonasToEntitiesResponse Prelude.Int
associatePersonasToEntitiesResponse_httpStatus = Lens.lens (\AssociatePersonasToEntitiesResponse' {httpStatus} -> httpStatus) (\s@AssociatePersonasToEntitiesResponse' {} a -> s {httpStatus = a} :: AssociatePersonasToEntitiesResponse)

instance
  Prelude.NFData
    AssociatePersonasToEntitiesResponse
  where
  rnf AssociatePersonasToEntitiesResponse' {..} =
    Prelude.rnf failedEntityList
      `Prelude.seq` Prelude.rnf httpStatus
