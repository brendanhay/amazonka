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
-- Module      : Amazonka.ChimeSDKIdentity.DeleteAppInstanceBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @AppInstanceBot@.
module Amazonka.ChimeSDKIdentity.DeleteAppInstanceBot
  ( -- * Creating a Request
    DeleteAppInstanceBot (..),
    newDeleteAppInstanceBot,

    -- * Request Lenses
    deleteAppInstanceBot_appInstanceBotArn,

    -- * Destructuring the Response
    DeleteAppInstanceBotResponse (..),
    newDeleteAppInstanceBotResponse,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppInstanceBot' smart constructor.
data DeleteAppInstanceBot = DeleteAppInstanceBot'
  { -- | The ARN of the @AppInstanceBot@ being deleted.
    appInstanceBotArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBotArn', 'deleteAppInstanceBot_appInstanceBotArn' - The ARN of the @AppInstanceBot@ being deleted.
newDeleteAppInstanceBot ::
  -- | 'appInstanceBotArn'
  Prelude.Text ->
  DeleteAppInstanceBot
newDeleteAppInstanceBot pAppInstanceBotArn_ =
  DeleteAppInstanceBot'
    { appInstanceBotArn =
        pAppInstanceBotArn_
    }

-- | The ARN of the @AppInstanceBot@ being deleted.
deleteAppInstanceBot_appInstanceBotArn :: Lens.Lens' DeleteAppInstanceBot Prelude.Text
deleteAppInstanceBot_appInstanceBotArn = Lens.lens (\DeleteAppInstanceBot' {appInstanceBotArn} -> appInstanceBotArn) (\s@DeleteAppInstanceBot' {} a -> s {appInstanceBotArn = a} :: DeleteAppInstanceBot)

instance Core.AWSRequest DeleteAppInstanceBot where
  type
    AWSResponse DeleteAppInstanceBot =
      DeleteAppInstanceBotResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteAppInstanceBotResponse'

instance Prelude.Hashable DeleteAppInstanceBot where
  hashWithSalt _salt DeleteAppInstanceBot' {..} =
    _salt `Prelude.hashWithSalt` appInstanceBotArn

instance Prelude.NFData DeleteAppInstanceBot where
  rnf DeleteAppInstanceBot' {..} =
    Prelude.rnf appInstanceBotArn

instance Data.ToHeaders DeleteAppInstanceBot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAppInstanceBot where
  toPath DeleteAppInstanceBot' {..} =
    Prelude.mconcat
      ["/app-instance-bots/", Data.toBS appInstanceBotArn]

instance Data.ToQuery DeleteAppInstanceBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppInstanceBotResponse' smart constructor.
data DeleteAppInstanceBotResponse = DeleteAppInstanceBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppInstanceBotResponse ::
  DeleteAppInstanceBotResponse
newDeleteAppInstanceBotResponse =
  DeleteAppInstanceBotResponse'

instance Prelude.NFData DeleteAppInstanceBotResponse where
  rnf _ = ()
