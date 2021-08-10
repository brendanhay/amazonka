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
-- Module      : Network.AWS.SageMaker.DeleteModelPackageGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified model group.
module Network.AWS.SageMaker.DeleteModelPackageGroup
  ( -- * Creating a Request
    DeleteModelPackageGroup (..),
    newDeleteModelPackageGroup,

    -- * Request Lenses
    deleteModelPackageGroup_modelPackageGroupName,

    -- * Destructuring the Response
    DeleteModelPackageGroupResponse (..),
    newDeleteModelPackageGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModelPackageGroup' smart constructor.
data DeleteModelPackageGroup = DeleteModelPackageGroup'
  { -- | The name of the model group to delete.
    modelPackageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'deleteModelPackageGroup_modelPackageGroupName' - The name of the model group to delete.
newDeleteModelPackageGroup ::
  -- | 'modelPackageGroupName'
  Prelude.Text ->
  DeleteModelPackageGroup
newDeleteModelPackageGroup pModelPackageGroupName_ =
  DeleteModelPackageGroup'
    { modelPackageGroupName =
        pModelPackageGroupName_
    }

-- | The name of the model group to delete.
deleteModelPackageGroup_modelPackageGroupName :: Lens.Lens' DeleteModelPackageGroup Prelude.Text
deleteModelPackageGroup_modelPackageGroupName = Lens.lens (\DeleteModelPackageGroup' {modelPackageGroupName} -> modelPackageGroupName) (\s@DeleteModelPackageGroup' {} a -> s {modelPackageGroupName = a} :: DeleteModelPackageGroup)

instance Core.AWSRequest DeleteModelPackageGroup where
  type
    AWSResponse DeleteModelPackageGroup =
      DeleteModelPackageGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteModelPackageGroupResponse'

instance Prelude.Hashable DeleteModelPackageGroup

instance Prelude.NFData DeleteModelPackageGroup

instance Core.ToHeaders DeleteModelPackageGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteModelPackageGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteModelPackageGroup where
  toJSON DeleteModelPackageGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ModelPackageGroupName"
                  Core..= modelPackageGroupName
              )
          ]
      )

instance Core.ToPath DeleteModelPackageGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteModelPackageGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelPackageGroupResponse' smart constructor.
data DeleteModelPackageGroupResponse = DeleteModelPackageGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackageGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelPackageGroupResponse ::
  DeleteModelPackageGroupResponse
newDeleteModelPackageGroupResponse =
  DeleteModelPackageGroupResponse'

instance
  Prelude.NFData
    DeleteModelPackageGroupResponse
