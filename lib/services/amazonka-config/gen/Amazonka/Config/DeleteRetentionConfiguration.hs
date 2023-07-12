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
-- Module      : Amazonka.Config.DeleteRetentionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the retention configuration.
module Amazonka.Config.DeleteRetentionConfiguration
  ( -- * Creating a Request
    DeleteRetentionConfiguration (..),
    newDeleteRetentionConfiguration,

    -- * Request Lenses
    deleteRetentionConfiguration_retentionConfigurationName,

    -- * Destructuring the Response
    DeleteRetentionConfigurationResponse (..),
    newDeleteRetentionConfigurationResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRetentionConfiguration' smart constructor.
data DeleteRetentionConfiguration = DeleteRetentionConfiguration'
  { -- | The name of the retention configuration to delete.
    retentionConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRetentionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionConfigurationName', 'deleteRetentionConfiguration_retentionConfigurationName' - The name of the retention configuration to delete.
newDeleteRetentionConfiguration ::
  -- | 'retentionConfigurationName'
  Prelude.Text ->
  DeleteRetentionConfiguration
newDeleteRetentionConfiguration
  pRetentionConfigurationName_ =
    DeleteRetentionConfiguration'
      { retentionConfigurationName =
          pRetentionConfigurationName_
      }

-- | The name of the retention configuration to delete.
deleteRetentionConfiguration_retentionConfigurationName :: Lens.Lens' DeleteRetentionConfiguration Prelude.Text
deleteRetentionConfiguration_retentionConfigurationName = Lens.lens (\DeleteRetentionConfiguration' {retentionConfigurationName} -> retentionConfigurationName) (\s@DeleteRetentionConfiguration' {} a -> s {retentionConfigurationName = a} :: DeleteRetentionConfiguration)

instance Core.AWSRequest DeleteRetentionConfiguration where
  type
    AWSResponse DeleteRetentionConfiguration =
      DeleteRetentionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRetentionConfigurationResponse'

instance
  Prelude.Hashable
    DeleteRetentionConfiguration
  where
  hashWithSalt _salt DeleteRetentionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` retentionConfigurationName

instance Prelude.NFData DeleteRetentionConfiguration where
  rnf DeleteRetentionConfiguration' {..} =
    Prelude.rnf retentionConfigurationName

instance Data.ToHeaders DeleteRetentionConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteRetentionConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRetentionConfiguration where
  toJSON DeleteRetentionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RetentionConfigurationName"
                  Data..= retentionConfigurationName
              )
          ]
      )

instance Data.ToPath DeleteRetentionConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRetentionConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRetentionConfigurationResponse' smart constructor.
data DeleteRetentionConfigurationResponse = DeleteRetentionConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRetentionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRetentionConfigurationResponse ::
  DeleteRetentionConfigurationResponse
newDeleteRetentionConfigurationResponse =
  DeleteRetentionConfigurationResponse'

instance
  Prelude.NFData
    DeleteRetentionConfigurationResponse
  where
  rnf _ = ()
