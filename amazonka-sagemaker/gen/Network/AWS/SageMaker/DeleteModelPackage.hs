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
-- Module      : Network.AWS.SageMaker.DeleteModelPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model package.
--
-- A model package is used to create Amazon SageMaker models or list on AWS
-- Marketplace. Buyers can subscribe to model packages listed on AWS
-- Marketplace to create models in Amazon SageMaker.
module Network.AWS.SageMaker.DeleteModelPackage
  ( -- * Creating a Request
    DeleteModelPackage (..),
    newDeleteModelPackage,

    -- * Request Lenses
    deleteModelPackage_modelPackageName,

    -- * Destructuring the Response
    DeleteModelPackageResponse (..),
    newDeleteModelPackageResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModelPackage' smart constructor.
data DeleteModelPackage = DeleteModelPackage'
  { -- | The name of the model package. The name must have 1 to 63 characters.
    -- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    modelPackageName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageName', 'deleteModelPackage_modelPackageName' - The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
newDeleteModelPackage ::
  -- | 'modelPackageName'
  Core.Text ->
  DeleteModelPackage
newDeleteModelPackage pModelPackageName_ =
  DeleteModelPackage'
    { modelPackageName =
        pModelPackageName_
    }

-- | The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
deleteModelPackage_modelPackageName :: Lens.Lens' DeleteModelPackage Core.Text
deleteModelPackage_modelPackageName = Lens.lens (\DeleteModelPackage' {modelPackageName} -> modelPackageName) (\s@DeleteModelPackage' {} a -> s {modelPackageName = a} :: DeleteModelPackage)

instance Core.AWSRequest DeleteModelPackage where
  type
    AWSResponse DeleteModelPackage =
      DeleteModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteModelPackageResponse'

instance Core.Hashable DeleteModelPackage

instance Core.NFData DeleteModelPackage

instance Core.ToHeaders DeleteModelPackage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteModelPackage" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteModelPackage where
  toJSON DeleteModelPackage' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ModelPackageName" Core..= modelPackageName)
          ]
      )

instance Core.ToPath DeleteModelPackage where
  toPath = Core.const "/"

instance Core.ToQuery DeleteModelPackage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteModelPackageResponse' smart constructor.
data DeleteModelPackageResponse = DeleteModelPackageResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelPackageResponse ::
  DeleteModelPackageResponse
newDeleteModelPackageResponse =
  DeleteModelPackageResponse'

instance Core.NFData DeleteModelPackageResponse
