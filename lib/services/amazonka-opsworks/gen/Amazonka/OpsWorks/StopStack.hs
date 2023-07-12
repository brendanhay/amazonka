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
-- Module      : Amazonka.OpsWorks.StopStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.StopStack
  ( -- * Creating a Request
    StopStack (..),
    newStopStack,

    -- * Request Lenses
    stopStack_stackId,

    -- * Destructuring the Response
    StopStackResponse (..),
    newStopStackResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopStack' smart constructor.
data StopStack = StopStack'
  { -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'stopStack_stackId' - The stack ID.
newStopStack ::
  -- | 'stackId'
  Prelude.Text ->
  StopStack
newStopStack pStackId_ =
  StopStack' {stackId = pStackId_}

-- | The stack ID.
stopStack_stackId :: Lens.Lens' StopStack Prelude.Text
stopStack_stackId = Lens.lens (\StopStack' {stackId} -> stackId) (\s@StopStack' {} a -> s {stackId = a} :: StopStack)

instance Core.AWSRequest StopStack where
  type AWSResponse StopStack = StopStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StopStackResponse'

instance Prelude.Hashable StopStack where
  hashWithSalt _salt StopStack' {..} =
    _salt `Prelude.hashWithSalt` stackId

instance Prelude.NFData StopStack where
  rnf StopStack' {..} = Prelude.rnf stackId

instance Data.ToHeaders StopStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.StopStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopStack where
  toJSON StopStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Data..= stackId)]
      )

instance Data.ToPath StopStack where
  toPath = Prelude.const "/"

instance Data.ToQuery StopStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopStackResponse' smart constructor.
data StopStackResponse = StopStackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopStackResponse ::
  StopStackResponse
newStopStackResponse = StopStackResponse'

instance Prelude.NFData StopStackResponse where
  rnf _ = ()
