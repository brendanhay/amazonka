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
-- Module      : Network.AWS.OpsWorks.StopStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorks.StopStack
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopStack' smart constructor.
data StopStack = StopStack'
  { -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopStack where
  type Rs StopStack = StopStackResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull StopStackResponse'

instance Prelude.Hashable StopStack

instance Prelude.NFData StopStack

instance Prelude.ToHeaders StopStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.StopStack" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopStack where
  toJSON StopStack' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Prelude..= stackId)]
      )

instance Prelude.ToPath StopStack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopStackResponse' smart constructor.
data StopStackResponse = StopStackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopStackResponse ::
  StopStackResponse
newStopStackResponse = StopStackResponse'

instance Prelude.NFData StopStackResponse
