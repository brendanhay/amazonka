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
-- Module      : Network.AWS.Config.DeleteRetentionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the retention configuration.
module Network.AWS.Config.DeleteRetentionConfiguration
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRetentionConfiguration' smart constructor.
data DeleteRetentionConfiguration = DeleteRetentionConfiguration'
  { -- | The name of the retention configuration to delete.
    retentionConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeleteRetentionConfiguration
  where
  type
    Rs DeleteRetentionConfiguration =
      DeleteRetentionConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteRetentionConfigurationResponse'

instance
  Prelude.Hashable
    DeleteRetentionConfiguration

instance Prelude.NFData DeleteRetentionConfiguration

instance
  Prelude.ToHeaders
    DeleteRetentionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteRetentionConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRetentionConfiguration where
  toJSON DeleteRetentionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RetentionConfigurationName"
                  Prelude..= retentionConfigurationName
              )
          ]
      )

instance Prelude.ToPath DeleteRetentionConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRetentionConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRetentionConfigurationResponse' smart constructor.
data DeleteRetentionConfigurationResponse = DeleteRetentionConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
