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
-- Module      : Amazonka.IVSChat.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified logging configuration.
module Amazonka.IVSChat.DeleteLoggingConfiguration
  ( -- * Creating a Request
    DeleteLoggingConfiguration (..),
    newDeleteLoggingConfiguration,

    -- * Request Lenses
    deleteLoggingConfiguration_identifier,

    -- * Destructuring the Response
    DeleteLoggingConfigurationResponse (..),
    newDeleteLoggingConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLoggingConfiguration' smart constructor.
data DeleteLoggingConfiguration = DeleteLoggingConfiguration'
  { -- | Identifier of the logging configuration to be deleted.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'deleteLoggingConfiguration_identifier' - Identifier of the logging configuration to be deleted.
newDeleteLoggingConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteLoggingConfiguration
newDeleteLoggingConfiguration pIdentifier_ =
  DeleteLoggingConfiguration'
    { identifier =
        pIdentifier_
    }

-- | Identifier of the logging configuration to be deleted.
deleteLoggingConfiguration_identifier :: Lens.Lens' DeleteLoggingConfiguration Prelude.Text
deleteLoggingConfiguration_identifier = Lens.lens (\DeleteLoggingConfiguration' {identifier} -> identifier) (\s@DeleteLoggingConfiguration' {} a -> s {identifier = a} :: DeleteLoggingConfiguration)

instance Core.AWSRequest DeleteLoggingConfiguration where
  type
    AWSResponse DeleteLoggingConfiguration =
      DeleteLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteLoggingConfigurationResponse'

instance Prelude.Hashable DeleteLoggingConfiguration where
  hashWithSalt _salt DeleteLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteLoggingConfiguration where
  rnf DeleteLoggingConfiguration' {..} =
    Prelude.rnf identifier

instance Data.ToHeaders DeleteLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLoggingConfiguration where
  toJSON DeleteLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("identifier" Data..= identifier)]
      )

instance Data.ToPath DeleteLoggingConfiguration where
  toPath = Prelude.const "/DeleteLoggingConfiguration"

instance Data.ToQuery DeleteLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLoggingConfigurationResponse' smart constructor.
data DeleteLoggingConfigurationResponse = DeleteLoggingConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLoggingConfigurationResponse ::
  DeleteLoggingConfigurationResponse
newDeleteLoggingConfigurationResponse =
  DeleteLoggingConfigurationResponse'

instance
  Prelude.NFData
    DeleteLoggingConfigurationResponse
  where
  rnf _ = ()
