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
-- Module      : Amazonka.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Redshift HSM configuration.
module Amazonka.Redshift.DeleteHsmConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteHsmConfiguration' smart constructor.
data DeleteHsmConfiguration = DeleteHsmConfiguration'
  { -- | The identifier of the Amazon Redshift HSM configuration to be deleted.
    hsmConfigurationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteHsmConfiguration where
  type
    AWSResponse DeleteHsmConfiguration =
      DeleteHsmConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteHsmConfigurationResponse'

instance Prelude.Hashable DeleteHsmConfiguration where
  hashWithSalt _salt DeleteHsmConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` hsmConfigurationIdentifier

instance Prelude.NFData DeleteHsmConfiguration where
  rnf DeleteHsmConfiguration' {..} =
    Prelude.rnf hsmConfigurationIdentifier

instance Core.ToHeaders DeleteHsmConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteHsmConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteHsmConfiguration where
  toQuery DeleteHsmConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteHsmConfiguration" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "HsmConfigurationIdentifier"
          Core.=: hsmConfigurationIdentifier
      ]

-- | /See:/ 'newDeleteHsmConfigurationResponse' smart constructor.
data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
