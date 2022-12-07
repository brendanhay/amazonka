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
-- Module      : Amazonka.ChimeSDKIdentity.DeleteAppInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @AppInstance@ and all associated data asynchronously.
module Amazonka.ChimeSDKIdentity.DeleteAppInstance
  ( -- * Creating a Request
    DeleteAppInstance (..),
    newDeleteAppInstance,

    -- * Request Lenses
    deleteAppInstance_appInstanceArn,

    -- * Destructuring the Response
    DeleteAppInstanceResponse (..),
    newDeleteAppInstanceResponse,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppInstance' smart constructor.
data DeleteAppInstance = DeleteAppInstance'
  { -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'deleteAppInstance_appInstanceArn' - The ARN of the @AppInstance@.
newDeleteAppInstance ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  DeleteAppInstance
newDeleteAppInstance pAppInstanceArn_ =
  DeleteAppInstance'
    { appInstanceArn =
        pAppInstanceArn_
    }

-- | The ARN of the @AppInstance@.
deleteAppInstance_appInstanceArn :: Lens.Lens' DeleteAppInstance Prelude.Text
deleteAppInstance_appInstanceArn = Lens.lens (\DeleteAppInstance' {appInstanceArn} -> appInstanceArn) (\s@DeleteAppInstance' {} a -> s {appInstanceArn = a} :: DeleteAppInstance)

instance Core.AWSRequest DeleteAppInstance where
  type
    AWSResponse DeleteAppInstance =
      DeleteAppInstanceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteAppInstanceResponse'

instance Prelude.Hashable DeleteAppInstance where
  hashWithSalt _salt DeleteAppInstance' {..} =
    _salt `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData DeleteAppInstance where
  rnf DeleteAppInstance' {..} =
    Prelude.rnf appInstanceArn

instance Data.ToHeaders DeleteAppInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAppInstance where
  toPath DeleteAppInstance' {..} =
    Prelude.mconcat
      ["/app-instances/", Data.toBS appInstanceArn]

instance Data.ToQuery DeleteAppInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppInstanceResponse' smart constructor.
data DeleteAppInstanceResponse = DeleteAppInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppInstanceResponse ::
  DeleteAppInstanceResponse
newDeleteAppInstanceResponse =
  DeleteAppInstanceResponse'

instance Prelude.NFData DeleteAppInstanceResponse where
  rnf _ = ()
