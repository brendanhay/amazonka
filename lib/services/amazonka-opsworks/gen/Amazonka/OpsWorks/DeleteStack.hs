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
-- Module      : Amazonka.OpsWorks.DeleteStack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified stack. You must first delete all instances, layers,
-- and apps or deregister registered instances. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html Shut Down a Stack>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DeleteStack
  ( -- * Creating a Request
    DeleteStack (..),
    newDeleteStack,

    -- * Request Lenses
    deleteStack_stackId,

    -- * Destructuring the Response
    DeleteStackResponse (..),
    newDeleteStackResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStack' smart constructor.
data DeleteStack = DeleteStack'
  { -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'deleteStack_stackId' - The stack ID.
newDeleteStack ::
  -- | 'stackId'
  Prelude.Text ->
  DeleteStack
newDeleteStack pStackId_ =
  DeleteStack' {stackId = pStackId_}

-- | The stack ID.
deleteStack_stackId :: Lens.Lens' DeleteStack Prelude.Text
deleteStack_stackId = Lens.lens (\DeleteStack' {stackId} -> stackId) (\s@DeleteStack' {} a -> s {stackId = a} :: DeleteStack)

instance Core.AWSRequest DeleteStack where
  type AWSResponse DeleteStack = DeleteStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteStackResponse'

instance Prelude.Hashable DeleteStack where
  hashWithSalt _salt DeleteStack' {..} =
    _salt `Prelude.hashWithSalt` stackId

instance Prelude.NFData DeleteStack where
  rnf DeleteStack' {..} = Prelude.rnf stackId

instance Data.ToHeaders DeleteStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DeleteStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStack where
  toJSON DeleteStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Data..= stackId)]
      )

instance Data.ToPath DeleteStack where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStackResponse' smart constructor.
data DeleteStackResponse = DeleteStackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStackResponse ::
  DeleteStackResponse
newDeleteStackResponse = DeleteStackResponse'

instance Prelude.NFData DeleteStackResponse where
  rnf _ = ()
