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
-- Module      : Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an on-premises instance.
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
  ( -- * Creating a Request
    DeregisterOnPremisesInstance (..),
    newDeregisterOnPremisesInstance,

    -- * Request Lenses
    deregisterOnPremisesInstance_instanceName,

    -- * Destructuring the Response
    DeregisterOnPremisesInstanceResponse (..),
    newDeregisterOnPremisesInstanceResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeregisterOnPremisesInstance@ operation.
--
-- /See:/ 'newDeregisterOnPremisesInstance' smart constructor.
data DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
  { -- | The name of the on-premises instance to deregister.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterOnPremisesInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceName', 'deregisterOnPremisesInstance_instanceName' - The name of the on-premises instance to deregister.
newDeregisterOnPremisesInstance ::
  -- | 'instanceName'
  Core.Text ->
  DeregisterOnPremisesInstance
newDeregisterOnPremisesInstance pInstanceName_ =
  DeregisterOnPremisesInstance'
    { instanceName =
        pInstanceName_
    }

-- | The name of the on-premises instance to deregister.
deregisterOnPremisesInstance_instanceName :: Lens.Lens' DeregisterOnPremisesInstance Core.Text
deregisterOnPremisesInstance_instanceName = Lens.lens (\DeregisterOnPremisesInstance' {instanceName} -> instanceName) (\s@DeregisterOnPremisesInstance' {} a -> s {instanceName = a} :: DeregisterOnPremisesInstance)

instance Core.AWSRequest DeregisterOnPremisesInstance where
  type
    AWSResponse DeregisterOnPremisesInstance =
      DeregisterOnPremisesInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeregisterOnPremisesInstanceResponse'

instance Core.Hashable DeregisterOnPremisesInstance

instance Core.NFData DeregisterOnPremisesInstance

instance Core.ToHeaders DeregisterOnPremisesInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.DeregisterOnPremisesInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterOnPremisesInstance where
  toJSON DeregisterOnPremisesInstance' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("instanceName" Core..= instanceName)]
      )

instance Core.ToPath DeregisterOnPremisesInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterOnPremisesInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterOnPremisesInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterOnPremisesInstanceResponse ::
  DeregisterOnPremisesInstanceResponse
newDeregisterOnPremisesInstanceResponse =
  DeregisterOnPremisesInstanceResponse'

instance
  Core.NFData
    DeregisterOnPremisesInstanceResponse
