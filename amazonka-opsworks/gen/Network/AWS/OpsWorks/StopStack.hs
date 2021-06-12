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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopStack' smart constructor.
data StopStack = StopStack'
  { -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StopStack
newStopStack pStackId_ =
  StopStack' {stackId = pStackId_}

-- | The stack ID.
stopStack_stackId :: Lens.Lens' StopStack Core.Text
stopStack_stackId = Lens.lens (\StopStack' {stackId} -> stackId) (\s@StopStack' {} a -> s {stackId = a} :: StopStack)

instance Core.AWSRequest StopStack where
  type AWSResponse StopStack = StopStackResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull StopStackResponse'

instance Core.Hashable StopStack

instance Core.NFData StopStack

instance Core.ToHeaders StopStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.StopStack" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopStack where
  toJSON StopStack' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("StackId" Core..= stackId)]
      )

instance Core.ToPath StopStack where
  toPath = Core.const "/"

instance Core.ToQuery StopStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopStackResponse' smart constructor.
data StopStackResponse = StopStackResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopStackResponse ::
  StopStackResponse
newStopStackResponse = StopStackResponse'

instance Core.NFData StopStackResponse
