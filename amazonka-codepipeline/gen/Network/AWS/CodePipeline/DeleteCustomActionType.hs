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
-- Module      : Network.AWS.CodePipeline.DeleteCustomActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks a custom action as deleted. @PollForJobs@ for the custom action
-- fails after the action is marked for deletion. Used for custom actions
-- only.
--
-- To re-create a custom action after it has been deleted you must use a
-- string in the version field that has never been used before. This string
-- can be an incremented version number, for example. To restore a deleted
-- custom action, use a JSON file that is identical to the deleted action,
-- including the original string in the version field.
module Network.AWS.CodePipeline.DeleteCustomActionType
  ( -- * Creating a Request
    DeleteCustomActionType (..),
    newDeleteCustomActionType,

    -- * Request Lenses
    deleteCustomActionType_category,
    deleteCustomActionType_provider,
    deleteCustomActionType_version,

    -- * Destructuring the Response
    DeleteCustomActionTypeResponse (..),
    newDeleteCustomActionTypeResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCustomActionType@ operation. The custom
-- action will be marked as deleted.
--
-- /See:/ 'newDeleteCustomActionType' smart constructor.
data DeleteCustomActionType = DeleteCustomActionType'
  { -- | The category of the custom action that you want to delete, such as
    -- source or deploy.
    category :: ActionCategory,
    -- | The provider of the service used in the custom action, such as AWS
    -- CodeDeploy.
    provider :: Core.Text,
    -- | The version of the custom action to delete.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCustomActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'deleteCustomActionType_category' - The category of the custom action that you want to delete, such as
-- source or deploy.
--
-- 'provider', 'deleteCustomActionType_provider' - The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
--
-- 'version', 'deleteCustomActionType_version' - The version of the custom action to delete.
newDeleteCustomActionType ::
  -- | 'category'
  ActionCategory ->
  -- | 'provider'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  DeleteCustomActionType
newDeleteCustomActionType
  pCategory_
  pProvider_
  pVersion_ =
    DeleteCustomActionType'
      { category = pCategory_,
        provider = pProvider_,
        version = pVersion_
      }

-- | The category of the custom action that you want to delete, such as
-- source or deploy.
deleteCustomActionType_category :: Lens.Lens' DeleteCustomActionType ActionCategory
deleteCustomActionType_category = Lens.lens (\DeleteCustomActionType' {category} -> category) (\s@DeleteCustomActionType' {} a -> s {category = a} :: DeleteCustomActionType)

-- | The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
deleteCustomActionType_provider :: Lens.Lens' DeleteCustomActionType Core.Text
deleteCustomActionType_provider = Lens.lens (\DeleteCustomActionType' {provider} -> provider) (\s@DeleteCustomActionType' {} a -> s {provider = a} :: DeleteCustomActionType)

-- | The version of the custom action to delete.
deleteCustomActionType_version :: Lens.Lens' DeleteCustomActionType Core.Text
deleteCustomActionType_version = Lens.lens (\DeleteCustomActionType' {version} -> version) (\s@DeleteCustomActionType' {} a -> s {version = a} :: DeleteCustomActionType)

instance Core.AWSRequest DeleteCustomActionType where
  type
    AWSResponse DeleteCustomActionType =
      DeleteCustomActionTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteCustomActionTypeResponse'

instance Core.Hashable DeleteCustomActionType

instance Core.NFData DeleteCustomActionType

instance Core.ToHeaders DeleteCustomActionType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.DeleteCustomActionType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteCustomActionType where
  toJSON DeleteCustomActionType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("category" Core..= category),
            Core.Just ("provider" Core..= provider),
            Core.Just ("version" Core..= version)
          ]
      )

instance Core.ToPath DeleteCustomActionType where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCustomActionType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCustomActionTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomActionTypeResponse ::
  DeleteCustomActionTypeResponse
newDeleteCustomActionTypeResponse =
  DeleteCustomActionTypeResponse'

instance Core.NFData DeleteCustomActionTypeResponse
