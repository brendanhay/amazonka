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
-- Module      : Amazonka.OpsWorks.StartStack
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.StartStack
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartStack' smart constructor.
data StartStack = StartStack'
  { -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartStack
newStartStack pStackId_ =
  StartStack' {stackId = pStackId_}

-- | The stack ID.
startStack_stackId :: Lens.Lens' StartStack Prelude.Text
startStack_stackId = Lens.lens (\StartStack' {stackId} -> stackId) (\s@StartStack' {} a -> s {stackId = a} :: StartStack)

instance Core.AWSRequest StartStack where
  type AWSResponse StartStack = StartStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StartStackResponse'

instance Prelude.Hashable StartStack where
  hashWithSalt _salt StartStack' {..} =
    _salt `Prelude.hashWithSalt` stackId

instance Prelude.NFData StartStack where
  rnf StartStack' {..} = Prelude.rnf stackId

instance Data.ToHeaders StartStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.StartStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartStack where
  toJSON StartStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Data..= stackId)]
      )

instance Data.ToPath StartStack where
  toPath = Prelude.const "/"

instance Data.ToQuery StartStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStackResponse' smart constructor.
data StartStackResponse = StartStackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartStackResponse ::
  StartStackResponse
newStartStackResponse = StartStackResponse'

instance Prelude.NFData StartStackResponse where
  rnf _ = ()
