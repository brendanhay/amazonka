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
-- Module      : Amazonka.ApiGatewayV2.DeleteStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Stage.
module Amazonka.ApiGatewayV2.DeleteStage
  ( -- * Creating a Request
    DeleteStage (..),
    newDeleteStage,

    -- * Request Lenses
    deleteStage_stageName,
    deleteStage_apiId,

    -- * Destructuring the Response
    DeleteStageResponse (..),
    newDeleteStageResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStage' smart constructor.
data DeleteStage = DeleteStage'
  { -- | The stage name. Stage names can only contain alphanumeric characters,
    -- hyphens, and underscores. Maximum length is 128 characters.
    stageName :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'deleteStage_stageName' - The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
--
-- 'apiId', 'deleteStage_apiId' - The API identifier.
newDeleteStage ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  DeleteStage
newDeleteStage pStageName_ pApiId_ =
  DeleteStage'
    { stageName = pStageName_,
      apiId = pApiId_
    }

-- | The stage name. Stage names can only contain alphanumeric characters,
-- hyphens, and underscores. Maximum length is 128 characters.
deleteStage_stageName :: Lens.Lens' DeleteStage Prelude.Text
deleteStage_stageName = Lens.lens (\DeleteStage' {stageName} -> stageName) (\s@DeleteStage' {} a -> s {stageName = a} :: DeleteStage)

-- | The API identifier.
deleteStage_apiId :: Lens.Lens' DeleteStage Prelude.Text
deleteStage_apiId = Lens.lens (\DeleteStage' {apiId} -> apiId) (\s@DeleteStage' {} a -> s {apiId = a} :: DeleteStage)

instance Core.AWSRequest DeleteStage where
  type AWSResponse DeleteStage = DeleteStageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteStageResponse'

instance Prelude.Hashable DeleteStage where
  hashWithSalt _salt DeleteStage' {..} =
    _salt
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData DeleteStage where
  rnf DeleteStage' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders DeleteStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteStage where
  toPath DeleteStage' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/stages/",
        Data.toBS stageName
      ]

instance Data.ToQuery DeleteStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStageResponse' smart constructor.
data DeleteStageResponse = DeleteStageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStageResponse ::
  DeleteStageResponse
newDeleteStageResponse = DeleteStageResponse'

instance Prelude.NFData DeleteStageResponse where
  rnf _ = ()
