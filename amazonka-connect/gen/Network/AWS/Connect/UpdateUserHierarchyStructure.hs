{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.UpdateUserHierarchyStructure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the user hierarchy structure: add, remove, and rename user
-- hierarchy levels.
module Network.AWS.Connect.UpdateUserHierarchyStructure
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserHierarchyStructure' smart constructor.
data UpdateUserHierarchyStructure = UpdateUserHierarchyStructure'
  { -- | The hierarchy levels to update.
    hierarchyStructure :: HierarchyStructureUpdate,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'instanceId', 'updateUserHierarchyStructure_instanceId' - The identifier of the Amazon Connect instance.
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

-- | The identifier of the Amazon Connect instance.
updateUserHierarchyStructure_instanceId :: Lens.Lens' UpdateUserHierarchyStructure Prelude.Text
updateUserHierarchyStructure_instanceId = Lens.lens (\UpdateUserHierarchyStructure' {instanceId} -> instanceId) (\s@UpdateUserHierarchyStructure' {} a -> s {instanceId = a} :: UpdateUserHierarchyStructure)

instance
  Prelude.AWSRequest
    UpdateUserHierarchyStructure
  where
  type
    Rs UpdateUserHierarchyStructure =
      UpdateUserHierarchyStructureResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateUserHierarchyStructureResponse'

instance
  Prelude.Hashable
    UpdateUserHierarchyStructure

instance Prelude.NFData UpdateUserHierarchyStructure

instance
  Prelude.ToHeaders
    UpdateUserHierarchyStructure
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserHierarchyStructure where
  toJSON UpdateUserHierarchyStructure' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HierarchyStructure"
                  Prelude..= hierarchyStructure
              )
          ]
      )

instance Prelude.ToPath UpdateUserHierarchyStructure where
  toPath UpdateUserHierarchyStructure' {..} =
    Prelude.mconcat
      [ "/user-hierarchy-structure/",
        Prelude.toBS instanceId
      ]

instance Prelude.ToQuery UpdateUserHierarchyStructure where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserHierarchyStructureResponse' smart constructor.
data UpdateUserHierarchyStructureResponse = UpdateUserHierarchyStructureResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
