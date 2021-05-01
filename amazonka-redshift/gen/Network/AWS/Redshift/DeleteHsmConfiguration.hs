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
-- Module      : Network.AWS.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.DeleteHsmConfiguration
  ( -- * Creating a Request
    DeleteHsmConfiguration (..),
    newDeleteHsmConfiguration,

    -- * Request Lenses
    deleteHsmConfiguration_hsmConfigurationIdentifier,

    -- * Destructuring the Response
    DeleteHsmConfigurationResponse (..),
    newDeleteHsmConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteHsmConfiguration' smart constructor.
data DeleteHsmConfiguration = DeleteHsmConfiguration'
  { -- | The identifier of the Amazon Redshift HSM configuration to be deleted.
    hsmConfigurationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteHsmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmConfigurationIdentifier', 'deleteHsmConfiguration_hsmConfigurationIdentifier' - The identifier of the Amazon Redshift HSM configuration to be deleted.
newDeleteHsmConfiguration ::
  -- | 'hsmConfigurationIdentifier'
  Prelude.Text ->
  DeleteHsmConfiguration
newDeleteHsmConfiguration
  pHsmConfigurationIdentifier_ =
    DeleteHsmConfiguration'
      { hsmConfigurationIdentifier =
          pHsmConfigurationIdentifier_
      }

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
deleteHsmConfiguration_hsmConfigurationIdentifier :: Lens.Lens' DeleteHsmConfiguration Prelude.Text
deleteHsmConfiguration_hsmConfigurationIdentifier = Lens.lens (\DeleteHsmConfiguration' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@DeleteHsmConfiguration' {} a -> s {hsmConfigurationIdentifier = a} :: DeleteHsmConfiguration)

instance Prelude.AWSRequest DeleteHsmConfiguration where
  type
    Rs DeleteHsmConfiguration =
      DeleteHsmConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteHsmConfigurationResponse'

instance Prelude.Hashable DeleteHsmConfiguration

instance Prelude.NFData DeleteHsmConfiguration

instance Prelude.ToHeaders DeleteHsmConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteHsmConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteHsmConfiguration where
  toQuery DeleteHsmConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteHsmConfiguration" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "HsmConfigurationIdentifier"
          Prelude.=: hsmConfigurationIdentifier
      ]

-- | /See:/ 'newDeleteHsmConfigurationResponse' smart constructor.
data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteHsmConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHsmConfigurationResponse ::
  DeleteHsmConfigurationResponse
newDeleteHsmConfigurationResponse =
  DeleteHsmConfigurationResponse'

instance
  Prelude.NFData
    DeleteHsmConfigurationResponse
