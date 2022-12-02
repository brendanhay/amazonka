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
-- Module      : Amazonka.ChimeSDKIdentity.DeregisterAppInstanceUserEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an @AppInstanceUserEndpoint@.
module Amazonka.ChimeSDKIdentity.DeregisterAppInstanceUserEndpoint
  ( -- * Creating a Request
    DeregisterAppInstanceUserEndpoint (..),
    newDeregisterAppInstanceUserEndpoint,

    -- * Request Lenses
    deregisterAppInstanceUserEndpoint_appInstanceUserArn,
    deregisterAppInstanceUserEndpoint_endpointId,

    -- * Destructuring the Response
    DeregisterAppInstanceUserEndpointResponse (..),
    newDeregisterAppInstanceUserEndpointResponse,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterAppInstanceUserEndpoint' smart constructor.
data DeregisterAppInstanceUserEndpoint = DeregisterAppInstanceUserEndpoint'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Data.Sensitive Prelude.Text,
    -- | The unique identifier of the @AppInstanceUserEndpoint@.
    endpointId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterAppInstanceUserEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'deregisterAppInstanceUserEndpoint_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'endpointId', 'deregisterAppInstanceUserEndpoint_endpointId' - The unique identifier of the @AppInstanceUserEndpoint@.
newDeregisterAppInstanceUserEndpoint ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  DeregisterAppInstanceUserEndpoint
newDeregisterAppInstanceUserEndpoint
  pAppInstanceUserArn_
  pEndpointId_ =
    DeregisterAppInstanceUserEndpoint'
      { appInstanceUserArn =
          Data._Sensitive
            Lens.# pAppInstanceUserArn_,
        endpointId =
          Data._Sensitive Lens.# pEndpointId_
      }

-- | The ARN of the @AppInstanceUser@.
deregisterAppInstanceUserEndpoint_appInstanceUserArn :: Lens.Lens' DeregisterAppInstanceUserEndpoint Prelude.Text
deregisterAppInstanceUserEndpoint_appInstanceUserArn = Lens.lens (\DeregisterAppInstanceUserEndpoint' {appInstanceUserArn} -> appInstanceUserArn) (\s@DeregisterAppInstanceUserEndpoint' {} a -> s {appInstanceUserArn = a} :: DeregisterAppInstanceUserEndpoint) Prelude.. Data._Sensitive

-- | The unique identifier of the @AppInstanceUserEndpoint@.
deregisterAppInstanceUserEndpoint_endpointId :: Lens.Lens' DeregisterAppInstanceUserEndpoint Prelude.Text
deregisterAppInstanceUserEndpoint_endpointId = Lens.lens (\DeregisterAppInstanceUserEndpoint' {endpointId} -> endpointId) (\s@DeregisterAppInstanceUserEndpoint' {} a -> s {endpointId = a} :: DeregisterAppInstanceUserEndpoint) Prelude.. Data._Sensitive

instance
  Core.AWSRequest
    DeregisterAppInstanceUserEndpoint
  where
  type
    AWSResponse DeregisterAppInstanceUserEndpoint =
      DeregisterAppInstanceUserEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeregisterAppInstanceUserEndpointResponse'

instance
  Prelude.Hashable
    DeregisterAppInstanceUserEndpoint
  where
  hashWithSalt
    _salt
    DeregisterAppInstanceUserEndpoint' {..} =
      _salt `Prelude.hashWithSalt` appInstanceUserArn
        `Prelude.hashWithSalt` endpointId

instance
  Prelude.NFData
    DeregisterAppInstanceUserEndpoint
  where
  rnf DeregisterAppInstanceUserEndpoint' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf endpointId

instance
  Data.ToHeaders
    DeregisterAppInstanceUserEndpoint
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeregisterAppInstanceUserEndpoint
  where
  toPath DeregisterAppInstanceUserEndpoint' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Data.toBS appInstanceUserArn,
        "/endpoints/",
        Data.toBS endpointId
      ]

instance
  Data.ToQuery
    DeregisterAppInstanceUserEndpoint
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterAppInstanceUserEndpointResponse' smart constructor.
data DeregisterAppInstanceUserEndpointResponse = DeregisterAppInstanceUserEndpointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterAppInstanceUserEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterAppInstanceUserEndpointResponse ::
  DeregisterAppInstanceUserEndpointResponse
newDeregisterAppInstanceUserEndpointResponse =
  DeregisterAppInstanceUserEndpointResponse'

instance
  Prelude.NFData
    DeregisterAppInstanceUserEndpointResponse
  where
  rnf _ = ()
