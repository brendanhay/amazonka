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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeregisterOnPremisesInstance@ operation.
--
-- /See:/ 'newDeregisterOnPremisesInstance' smart constructor.
data DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
  { -- | The name of the on-premises instance to deregister.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeregisterOnPremisesInstance
newDeregisterOnPremisesInstance pInstanceName_ =
  DeregisterOnPremisesInstance'
    { instanceName =
        pInstanceName_
    }

-- | The name of the on-premises instance to deregister.
deregisterOnPremisesInstance_instanceName :: Lens.Lens' DeregisterOnPremisesInstance Prelude.Text
deregisterOnPremisesInstance_instanceName = Lens.lens (\DeregisterOnPremisesInstance' {instanceName} -> instanceName) (\s@DeregisterOnPremisesInstance' {} a -> s {instanceName = a} :: DeregisterOnPremisesInstance)

instance
  Prelude.AWSRequest
    DeregisterOnPremisesInstance
  where
  type
    Rs DeregisterOnPremisesInstance =
      DeregisterOnPremisesInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeregisterOnPremisesInstanceResponse'

instance
  Prelude.Hashable
    DeregisterOnPremisesInstance

instance Prelude.NFData DeregisterOnPremisesInstance

instance
  Prelude.ToHeaders
    DeregisterOnPremisesInstance
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.DeregisterOnPremisesInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterOnPremisesInstance where
  toJSON DeregisterOnPremisesInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("instanceName" Prelude..= instanceName)
          ]
      )

instance Prelude.ToPath DeregisterOnPremisesInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterOnPremisesInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOnPremisesInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterOnPremisesInstanceResponse ::
  DeregisterOnPremisesInstanceResponse
newDeregisterOnPremisesInstanceResponse =
  DeregisterOnPremisesInstanceResponse'

instance
  Prelude.NFData
    DeregisterOnPremisesInstanceResponse
