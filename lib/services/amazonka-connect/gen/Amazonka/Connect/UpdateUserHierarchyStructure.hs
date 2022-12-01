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
-- Module      : Amazonka.Connect.UpdateUserHierarchyStructure
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the user hierarchy structure: add, remove, and rename user
-- hierarchy levels.
module Amazonka.Connect.UpdateUserHierarchyStructure
  ( -- * Creating a Request
    UpdateUserHierarchyStructure (..),
    newUpdateUserHierarchyStructure,

    -- * Request Lenses
    updateUserHierarchyStructure_hierarchyStructure,
    updateUserHierarchyStructure_instanceId,

    -- * Destructuring the Response
    UpdateUserHierarchyStructureResponse (..),
    newUpdateUserHierarchyStructureResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserHierarchyStructure' smart constructor.
data UpdateUserHierarchyStructure = UpdateUserHierarchyStructure'
  { -- | The hierarchy levels to update.
    hierarchyStructure :: HierarchyStructureUpdate,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchyStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyStructure', 'updateUserHierarchyStructure_hierarchyStructure' - The hierarchy levels to update.
--
-- 'instanceId', 'updateUserHierarchyStructure_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newUpdateUserHierarchyStructure ::
  -- | 'hierarchyStructure'
  HierarchyStructureUpdate ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateUserHierarchyStructure
newUpdateUserHierarchyStructure
  pHierarchyStructure_
  pInstanceId_ =
    UpdateUserHierarchyStructure'
      { hierarchyStructure =
          pHierarchyStructure_,
        instanceId = pInstanceId_
      }

-- | The hierarchy levels to update.
updateUserHierarchyStructure_hierarchyStructure :: Lens.Lens' UpdateUserHierarchyStructure HierarchyStructureUpdate
updateUserHierarchyStructure_hierarchyStructure = Lens.lens (\UpdateUserHierarchyStructure' {hierarchyStructure} -> hierarchyStructure) (\s@UpdateUserHierarchyStructure' {} a -> s {hierarchyStructure = a} :: UpdateUserHierarchyStructure)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateUserHierarchyStructure_instanceId :: Lens.Lens' UpdateUserHierarchyStructure Prelude.Text
updateUserHierarchyStructure_instanceId = Lens.lens (\UpdateUserHierarchyStructure' {instanceId} -> instanceId) (\s@UpdateUserHierarchyStructure' {} a -> s {instanceId = a} :: UpdateUserHierarchyStructure)

instance Core.AWSRequest UpdateUserHierarchyStructure where
  type
    AWSResponse UpdateUserHierarchyStructure =
      UpdateUserHierarchyStructureResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateUserHierarchyStructureResponse'

instance
  Prelude.Hashable
    UpdateUserHierarchyStructure
  where
  hashWithSalt _salt UpdateUserHierarchyStructure' {..} =
    _salt `Prelude.hashWithSalt` hierarchyStructure
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateUserHierarchyStructure where
  rnf UpdateUserHierarchyStructure' {..} =
    Prelude.rnf hierarchyStructure
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders UpdateUserHierarchyStructure where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUserHierarchyStructure where
  toJSON UpdateUserHierarchyStructure' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HierarchyStructure" Core..= hierarchyStructure)
          ]
      )

instance Core.ToPath UpdateUserHierarchyStructure where
  toPath UpdateUserHierarchyStructure' {..} =
    Prelude.mconcat
      ["/user-hierarchy-structure/", Core.toBS instanceId]

instance Core.ToQuery UpdateUserHierarchyStructure where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserHierarchyStructureResponse' smart constructor.
data UpdateUserHierarchyStructureResponse = UpdateUserHierarchyStructureResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchyStructureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserHierarchyStructureResponse ::
  UpdateUserHierarchyStructureResponse
newUpdateUserHierarchyStructureResponse =
  UpdateUserHierarchyStructureResponse'

instance
  Prelude.NFData
    UpdateUserHierarchyStructureResponse
  where
  rnf _ = ()
