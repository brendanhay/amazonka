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
-- Module      : Amazonka.Config.DeleteResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for a custom resource that has been
-- deleted. This API records a new ConfigurationItem with a ResourceDeleted
-- status. You can retrieve the ConfigurationItems recorded for this
-- resource in your Config History.
module Amazonka.Config.DeleteResourceConfig
  ( -- * Creating a Request
    DeleteResourceConfig (..),
    newDeleteResourceConfig,

    -- * Request Lenses
    deleteResourceConfig_resourceType,
    deleteResourceConfig_resourceId,

    -- * Destructuring the Response
    DeleteResourceConfigResponse (..),
    newDeleteResourceConfigResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceConfig' smart constructor.
data DeleteResourceConfig = DeleteResourceConfig'
  { -- | The type of the resource.
    resourceType :: Prelude.Text,
    -- | Unique identifier of the resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'deleteResourceConfig_resourceType' - The type of the resource.
--
-- 'resourceId', 'deleteResourceConfig_resourceId' - Unique identifier of the resource.
newDeleteResourceConfig ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  DeleteResourceConfig
newDeleteResourceConfig pResourceType_ pResourceId_ =
  DeleteResourceConfig'
    { resourceType =
        pResourceType_,
      resourceId = pResourceId_
    }

-- | The type of the resource.
deleteResourceConfig_resourceType :: Lens.Lens' DeleteResourceConfig Prelude.Text
deleteResourceConfig_resourceType = Lens.lens (\DeleteResourceConfig' {resourceType} -> resourceType) (\s@DeleteResourceConfig' {} a -> s {resourceType = a} :: DeleteResourceConfig)

-- | Unique identifier of the resource.
deleteResourceConfig_resourceId :: Lens.Lens' DeleteResourceConfig Prelude.Text
deleteResourceConfig_resourceId = Lens.lens (\DeleteResourceConfig' {resourceId} -> resourceId) (\s@DeleteResourceConfig' {} a -> s {resourceId = a} :: DeleteResourceConfig)

instance Core.AWSRequest DeleteResourceConfig where
  type
    AWSResponse DeleteResourceConfig =
      DeleteResourceConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteResourceConfigResponse'

instance Prelude.Hashable DeleteResourceConfig where
  hashWithSalt _salt DeleteResourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DeleteResourceConfig where
  rnf DeleteResourceConfig' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders DeleteResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourceConfig where
  toJSON DeleteResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath DeleteResourceConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceConfigResponse' smart constructor.
data DeleteResourceConfigResponse = DeleteResourceConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceConfigResponse ::
  DeleteResourceConfigResponse
newDeleteResourceConfigResponse =
  DeleteResourceConfigResponse'

instance Prelude.NFData DeleteResourceConfigResponse where
  rnf _ = ()
