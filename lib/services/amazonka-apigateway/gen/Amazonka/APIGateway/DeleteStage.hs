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
-- Module      : Amazonka.APIGateway.DeleteStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Stage resource.
module Amazonka.APIGateway.DeleteStage
  ( -- * Creating a Request
    DeleteStage (..),
    newDeleteStage,

    -- * Request Lenses
    deleteStage_restApiId,
    deleteStage_stageName,

    -- * Destructuring the Response
    DeleteStageResponse (..),
    newDeleteStageResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests API Gateway to delete a Stage resource.
--
-- /See:/ 'newDeleteStage' smart constructor.
data DeleteStage = DeleteStage'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the Stage resource to delete.
    stageName :: Prelude.Text
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
-- 'restApiId', 'deleteStage_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'deleteStage_stageName' - The name of the Stage resource to delete.
newDeleteStage ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  DeleteStage
newDeleteStage pRestApiId_ pStageName_ =
  DeleteStage'
    { restApiId = pRestApiId_,
      stageName = pStageName_
    }

-- | The string identifier of the associated RestApi.
deleteStage_restApiId :: Lens.Lens' DeleteStage Prelude.Text
deleteStage_restApiId = Lens.lens (\DeleteStage' {restApiId} -> restApiId) (\s@DeleteStage' {} a -> s {restApiId = a} :: DeleteStage)

-- | The name of the Stage resource to delete.
deleteStage_stageName :: Lens.Lens' DeleteStage Prelude.Text
deleteStage_stageName = Lens.lens (\DeleteStage' {stageName} -> stageName) (\s@DeleteStage' {} a -> s {stageName = a} :: DeleteStage)

instance Core.AWSRequest DeleteStage where
  type AWSResponse DeleteStage = DeleteStageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteStageResponse'

instance Prelude.Hashable DeleteStage where
  hashWithSalt _salt DeleteStage' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData DeleteStage where
  rnf DeleteStage' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders DeleteStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteStage where
  toPath DeleteStage' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
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
