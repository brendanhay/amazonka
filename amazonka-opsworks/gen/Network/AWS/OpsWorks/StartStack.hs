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
-- Module      : Network.AWS.OpsWorks.StartStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a stack\'s instances.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.StartStack
  ( -- * Creating a Request
    StartStack (..),
    newStartStack,

    -- * Request Lenses
    startStack_stackId,

    -- * Destructuring the Response
    StartStackResponse (..),
    newStartStackResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartStack' smart constructor.
data StartStack = StartStack'
  { -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'startStack_stackId' - The stack ID.
newStartStack ::
  -- | 'stackId'
  Core.Text ->
  StartStack
newStartStack pStackId_ =
  StartStack' {stackId = pStackId_}

-- | The stack ID.
startStack_stackId :: Lens.Lens' StartStack Core.Text
startStack_stackId = Lens.lens (\StartStack' {stackId} -> stackId) (\s@StartStack' {} a -> s {stackId = a} :: StartStack)

instance Core.AWSRequest StartStack where
  type AWSResponse StartStack = StartStackResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull StartStackResponse'

instance Core.Hashable StartStack

instance Core.NFData StartStack

instance Core.ToHeaders StartStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.StartStack" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartStack where
  toJSON StartStack' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("StackId" Core..= stackId)]
      )

instance Core.ToPath StartStack where
  toPath = Core.const "/"

instance Core.ToQuery StartStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartStackResponse' smart constructor.
data StartStackResponse = StartStackResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartStackResponse ::
  StartStackResponse
newStartStackResponse = StartStackResponse'

instance Core.NFData StartStackResponse
