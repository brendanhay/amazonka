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
-- Module      : Amazonka.SageMaker.DeleteModelPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model package.
--
-- A model package is used to create SageMaker models or list on Amazon Web
-- Services Marketplace. Buyers can subscribe to model packages listed on
-- Amazon Web Services Marketplace to create models in SageMaker.
module Amazonka.SageMaker.DeleteModelPackage
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteModelPackage' smart constructor.
data DeleteModelPackage = DeleteModelPackage'
  { -- | The name or Amazon Resource Name (ARN) of the model package to delete.
    --
    -- When you specify a name, the name must have 1 to 63 characters. Valid
    -- characters are a-z, A-Z, 0-9, and - (hyphen).
    modelPackageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageName', 'deleteModelPackage_modelPackageName' - The name or Amazon Resource Name (ARN) of the model package to delete.
--
-- When you specify a name, the name must have 1 to 63 characters. Valid
-- characters are a-z, A-Z, 0-9, and - (hyphen).
newDeleteModelPackage ::
  -- | 'modelPackageName'
  Prelude.Text ->
  DeleteModelPackage
newDeleteModelPackage pModelPackageName_ =
  DeleteModelPackage'
    { modelPackageName =
        pModelPackageName_
    }

-- | The name or Amazon Resource Name (ARN) of the model package to delete.
--
-- When you specify a name, the name must have 1 to 63 characters. Valid
-- characters are a-z, A-Z, 0-9, and - (hyphen).
deleteModelPackage_modelPackageName :: Lens.Lens' DeleteModelPackage Prelude.Text
deleteModelPackage_modelPackageName = Lens.lens (\DeleteModelPackage' {modelPackageName} -> modelPackageName) (\s@DeleteModelPackage' {} a -> s {modelPackageName = a} :: DeleteModelPackage)

instance Core.AWSRequest DeleteModelPackage where
  type
    AWSResponse DeleteModelPackage =
      DeleteModelPackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteModelPackageResponse'

instance Prelude.Hashable DeleteModelPackage where
  hashWithSalt _salt DeleteModelPackage' {..} =
    _salt `Prelude.hashWithSalt` modelPackageName

instance Prelude.NFData DeleteModelPackage where
  rnf DeleteModelPackage' {..} =
    Prelude.rnf modelPackageName

instance Data.ToHeaders DeleteModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteModelPackage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteModelPackage where
  toJSON DeleteModelPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ModelPackageName" Data..= modelPackageName)
          ]
      )

instance Data.ToPath DeleteModelPackage where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelPackageResponse' smart constructor.
data DeleteModelPackageResponse = DeleteModelPackageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelPackageResponse ::
  DeleteModelPackageResponse
newDeleteModelPackageResponse =
  DeleteModelPackageResponse'

instance Prelude.NFData DeleteModelPackageResponse where
  rnf _ = ()
