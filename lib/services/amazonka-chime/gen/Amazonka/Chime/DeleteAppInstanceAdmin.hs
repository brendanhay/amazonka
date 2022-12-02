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
-- Module      : Amazonka.Chime.DeleteAppInstanceAdmin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Demotes an @AppInstanceAdmin@ to an @AppInstanceUser@. This action does
-- not delete the user.
module Amazonka.Chime.DeleteAppInstanceAdmin
  ( -- * Creating a Request
    DeleteAppInstanceAdmin (..),
    newDeleteAppInstanceAdmin,

    -- * Request Lenses
    deleteAppInstanceAdmin_appInstanceAdminArn,
    deleteAppInstanceAdmin_appInstanceArn,

    -- * Destructuring the Response
    DeleteAppInstanceAdminResponse (..),
    newDeleteAppInstanceAdminResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppInstanceAdmin' smart constructor.
data DeleteAppInstanceAdmin = DeleteAppInstanceAdmin'
  { -- | The ARN of the @AppInstance@\'s administrator.
    appInstanceAdminArn :: Prelude.Text,
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceAdminArn', 'deleteAppInstanceAdmin_appInstanceAdminArn' - The ARN of the @AppInstance@\'s administrator.
--
-- 'appInstanceArn', 'deleteAppInstanceAdmin_appInstanceArn' - The ARN of the @AppInstance@.
newDeleteAppInstanceAdmin ::
  -- | 'appInstanceAdminArn'
  Prelude.Text ->
  -- | 'appInstanceArn'
  Prelude.Text ->
  DeleteAppInstanceAdmin
newDeleteAppInstanceAdmin
  pAppInstanceAdminArn_
  pAppInstanceArn_ =
    DeleteAppInstanceAdmin'
      { appInstanceAdminArn =
          pAppInstanceAdminArn_,
        appInstanceArn = pAppInstanceArn_
      }

-- | The ARN of the @AppInstance@\'s administrator.
deleteAppInstanceAdmin_appInstanceAdminArn :: Lens.Lens' DeleteAppInstanceAdmin Prelude.Text
deleteAppInstanceAdmin_appInstanceAdminArn = Lens.lens (\DeleteAppInstanceAdmin' {appInstanceAdminArn} -> appInstanceAdminArn) (\s@DeleteAppInstanceAdmin' {} a -> s {appInstanceAdminArn = a} :: DeleteAppInstanceAdmin)

-- | The ARN of the @AppInstance@.
deleteAppInstanceAdmin_appInstanceArn :: Lens.Lens' DeleteAppInstanceAdmin Prelude.Text
deleteAppInstanceAdmin_appInstanceArn = Lens.lens (\DeleteAppInstanceAdmin' {appInstanceArn} -> appInstanceArn) (\s@DeleteAppInstanceAdmin' {} a -> s {appInstanceArn = a} :: DeleteAppInstanceAdmin)

instance Core.AWSRequest DeleteAppInstanceAdmin where
  type
    AWSResponse DeleteAppInstanceAdmin =
      DeleteAppInstanceAdminResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAppInstanceAdminResponse'

instance Prelude.Hashable DeleteAppInstanceAdmin where
  hashWithSalt _salt DeleteAppInstanceAdmin' {..} =
    _salt `Prelude.hashWithSalt` appInstanceAdminArn
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData DeleteAppInstanceAdmin where
  rnf DeleteAppInstanceAdmin' {..} =
    Prelude.rnf appInstanceAdminArn
      `Prelude.seq` Prelude.rnf appInstanceArn

instance Data.ToHeaders DeleteAppInstanceAdmin where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAppInstanceAdmin where
  toPath DeleteAppInstanceAdmin' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Data.toBS appInstanceArn,
        "/admins/",
        Data.toBS appInstanceAdminArn
      ]

instance Data.ToQuery DeleteAppInstanceAdmin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppInstanceAdminResponse' smart constructor.
data DeleteAppInstanceAdminResponse = DeleteAppInstanceAdminResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppInstanceAdminResponse ::
  DeleteAppInstanceAdminResponse
newDeleteAppInstanceAdminResponse =
  DeleteAppInstanceAdminResponse'

instance
  Prelude.NFData
    DeleteAppInstanceAdminResponse
  where
  rnf _ = ()
