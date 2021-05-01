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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModelPackage' smart constructor.
data DeleteModelPackage = DeleteModelPackage'
  { -- | The name of the model package. The name must have 1 to 63 characters.
    -- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    modelPackageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteModelPackage
newDeleteModelPackage pModelPackageName_ =
  DeleteModelPackage'
    { modelPackageName =
        pModelPackageName_
    }

-- | The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
deleteModelPackage_modelPackageName :: Lens.Lens' DeleteModelPackage Prelude.Text
deleteModelPackage_modelPackageName = Lens.lens (\DeleteModelPackage' {modelPackageName} -> modelPackageName) (\s@DeleteModelPackage' {} a -> s {modelPackageName = a} :: DeleteModelPackage)

instance Prelude.AWSRequest DeleteModelPackage where
  type
    Rs DeleteModelPackage =
      DeleteModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteModelPackageResponse'

instance Prelude.Hashable DeleteModelPackage

instance Prelude.NFData DeleteModelPackage

instance Prelude.ToHeaders DeleteModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteModelPackage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteModelPackage where
  toJSON DeleteModelPackage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ModelPackageName" Prelude..= modelPackageName)
          ]
      )

instance Prelude.ToPath DeleteModelPackage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelPackageResponse' smart constructor.
data DeleteModelPackageResponse = DeleteModelPackageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelPackageResponse ::
  DeleteModelPackageResponse
newDeleteModelPackageResponse =
  DeleteModelPackageResponse'

instance Prelude.NFData DeleteModelPackageResponse
