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
-- Module      : Amazonka.CodeDeploy.DeregisterOnPremisesInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an on-premises instance.
module Amazonka.CodeDeploy.DeregisterOnPremisesInstance
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeregisterOnPremisesInstance@ operation.
--
-- /See:/ 'newDeregisterOnPremisesInstance' smart constructor.
data DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
  { -- | The name of the on-premises instance to deregister.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeregisterOnPremisesInstance where
  type
    AWSResponse DeregisterOnPremisesInstance =
      DeregisterOnPremisesInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeregisterOnPremisesInstanceResponse'

instance
  Prelude.Hashable
    DeregisterOnPremisesInstance
  where
  hashWithSalt _salt DeregisterOnPremisesInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceName

instance Prelude.NFData DeregisterOnPremisesInstance where
  rnf DeregisterOnPremisesInstance' {..} =
    Prelude.rnf instanceName

instance Data.ToHeaders DeregisterOnPremisesInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.DeregisterOnPremisesInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterOnPremisesInstance where
  toJSON DeregisterOnPremisesInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceName" Data..= instanceName)]
      )

instance Data.ToPath DeregisterOnPremisesInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterOnPremisesInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
