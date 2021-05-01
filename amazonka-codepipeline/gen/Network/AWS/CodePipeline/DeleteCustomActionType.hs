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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    provider :: Prelude.Text,
    -- | The version of the custom action to delete.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
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
deleteCustomActionType_provider :: Lens.Lens' DeleteCustomActionType Prelude.Text
deleteCustomActionType_provider = Lens.lens (\DeleteCustomActionType' {provider} -> provider) (\s@DeleteCustomActionType' {} a -> s {provider = a} :: DeleteCustomActionType)

-- | The version of the custom action to delete.
deleteCustomActionType_version :: Lens.Lens' DeleteCustomActionType Prelude.Text
deleteCustomActionType_version = Lens.lens (\DeleteCustomActionType' {version} -> version) (\s@DeleteCustomActionType' {} a -> s {version = a} :: DeleteCustomActionType)

instance Prelude.AWSRequest DeleteCustomActionType where
  type
    Rs DeleteCustomActionType =
      DeleteCustomActionTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteCustomActionTypeResponse'

instance Prelude.Hashable DeleteCustomActionType

instance Prelude.NFData DeleteCustomActionType

instance Prelude.ToHeaders DeleteCustomActionType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.DeleteCustomActionType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteCustomActionType where
  toJSON DeleteCustomActionType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("category" Prelude..= category),
            Prelude.Just ("provider" Prelude..= provider),
            Prelude.Just ("version" Prelude..= version)
          ]
      )

instance Prelude.ToPath DeleteCustomActionType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCustomActionType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomActionTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomActionTypeResponse ::
  DeleteCustomActionTypeResponse
newDeleteCustomActionTypeResponse =
  DeleteCustomActionTypeResponse'

instance
  Prelude.NFData
    DeleteCustomActionTypeResponse
