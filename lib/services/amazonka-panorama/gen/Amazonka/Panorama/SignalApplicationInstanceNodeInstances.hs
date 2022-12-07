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
-- Module      : Amazonka.Panorama.SignalApplicationInstanceNodeInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signal camera nodes to stop or resume.
module Amazonka.Panorama.SignalApplicationInstanceNodeInstances
  ( -- * Creating a Request
    SignalApplicationInstanceNodeInstances (..),
    newSignalApplicationInstanceNodeInstances,

    -- * Request Lenses
    signalApplicationInstanceNodeInstances_applicationInstanceId,
    signalApplicationInstanceNodeInstances_nodeSignals,

    -- * Destructuring the Response
    SignalApplicationInstanceNodeInstancesResponse (..),
    newSignalApplicationInstanceNodeInstancesResponse,

    -- * Response Lenses
    signalApplicationInstanceNodeInstancesResponse_httpStatus,
    signalApplicationInstanceNodeInstancesResponse_applicationInstanceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSignalApplicationInstanceNodeInstances' smart constructor.
data SignalApplicationInstanceNodeInstances = SignalApplicationInstanceNodeInstances'
  { -- | An application instance ID.
    applicationInstanceId :: Prelude.Text,
    -- | A list of signals.
    nodeSignals :: Prelude.NonEmpty NodeSignal
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalApplicationInstanceNodeInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceId', 'signalApplicationInstanceNodeInstances_applicationInstanceId' - An application instance ID.
--
-- 'nodeSignals', 'signalApplicationInstanceNodeInstances_nodeSignals' - A list of signals.
newSignalApplicationInstanceNodeInstances ::
  -- | 'applicationInstanceId'
  Prelude.Text ->
  -- | 'nodeSignals'
  Prelude.NonEmpty NodeSignal ->
  SignalApplicationInstanceNodeInstances
newSignalApplicationInstanceNodeInstances
  pApplicationInstanceId_
  pNodeSignals_ =
    SignalApplicationInstanceNodeInstances'
      { applicationInstanceId =
          pApplicationInstanceId_,
        nodeSignals =
          Lens.coerced Lens.# pNodeSignals_
      }

-- | An application instance ID.
signalApplicationInstanceNodeInstances_applicationInstanceId :: Lens.Lens' SignalApplicationInstanceNodeInstances Prelude.Text
signalApplicationInstanceNodeInstances_applicationInstanceId = Lens.lens (\SignalApplicationInstanceNodeInstances' {applicationInstanceId} -> applicationInstanceId) (\s@SignalApplicationInstanceNodeInstances' {} a -> s {applicationInstanceId = a} :: SignalApplicationInstanceNodeInstances)

-- | A list of signals.
signalApplicationInstanceNodeInstances_nodeSignals :: Lens.Lens' SignalApplicationInstanceNodeInstances (Prelude.NonEmpty NodeSignal)
signalApplicationInstanceNodeInstances_nodeSignals = Lens.lens (\SignalApplicationInstanceNodeInstances' {nodeSignals} -> nodeSignals) (\s@SignalApplicationInstanceNodeInstances' {} a -> s {nodeSignals = a} :: SignalApplicationInstanceNodeInstances) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    SignalApplicationInstanceNodeInstances
  where
  type
    AWSResponse
      SignalApplicationInstanceNodeInstances =
      SignalApplicationInstanceNodeInstancesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SignalApplicationInstanceNodeInstancesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "ApplicationInstanceId")
      )

instance
  Prelude.Hashable
    SignalApplicationInstanceNodeInstances
  where
  hashWithSalt
    _salt
    SignalApplicationInstanceNodeInstances' {..} =
      _salt `Prelude.hashWithSalt` applicationInstanceId
        `Prelude.hashWithSalt` nodeSignals

instance
  Prelude.NFData
    SignalApplicationInstanceNodeInstances
  where
  rnf SignalApplicationInstanceNodeInstances' {..} =
    Prelude.rnf applicationInstanceId
      `Prelude.seq` Prelude.rnf nodeSignals

instance
  Data.ToHeaders
    SignalApplicationInstanceNodeInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    SignalApplicationInstanceNodeInstances
  where
  toJSON SignalApplicationInstanceNodeInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NodeSignals" Data..= nodeSignals)]
      )

instance
  Data.ToPath
    SignalApplicationInstanceNodeInstances
  where
  toPath SignalApplicationInstanceNodeInstances' {..} =
    Prelude.mconcat
      [ "/application-instances/",
        Data.toBS applicationInstanceId,
        "/node-signals"
      ]

instance
  Data.ToQuery
    SignalApplicationInstanceNodeInstances
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSignalApplicationInstanceNodeInstancesResponse' smart constructor.
data SignalApplicationInstanceNodeInstancesResponse = SignalApplicationInstanceNodeInstancesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An application instance ID.
    applicationInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalApplicationInstanceNodeInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'signalApplicationInstanceNodeInstancesResponse_httpStatus' - The response's http status code.
--
-- 'applicationInstanceId', 'signalApplicationInstanceNodeInstancesResponse_applicationInstanceId' - An application instance ID.
newSignalApplicationInstanceNodeInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationInstanceId'
  Prelude.Text ->
  SignalApplicationInstanceNodeInstancesResponse
newSignalApplicationInstanceNodeInstancesResponse
  pHttpStatus_
  pApplicationInstanceId_ =
    SignalApplicationInstanceNodeInstancesResponse'
      { httpStatus =
          pHttpStatus_,
        applicationInstanceId =
          pApplicationInstanceId_
      }

-- | The response's http status code.
signalApplicationInstanceNodeInstancesResponse_httpStatus :: Lens.Lens' SignalApplicationInstanceNodeInstancesResponse Prelude.Int
signalApplicationInstanceNodeInstancesResponse_httpStatus = Lens.lens (\SignalApplicationInstanceNodeInstancesResponse' {httpStatus} -> httpStatus) (\s@SignalApplicationInstanceNodeInstancesResponse' {} a -> s {httpStatus = a} :: SignalApplicationInstanceNodeInstancesResponse)

-- | An application instance ID.
signalApplicationInstanceNodeInstancesResponse_applicationInstanceId :: Lens.Lens' SignalApplicationInstanceNodeInstancesResponse Prelude.Text
signalApplicationInstanceNodeInstancesResponse_applicationInstanceId = Lens.lens (\SignalApplicationInstanceNodeInstancesResponse' {applicationInstanceId} -> applicationInstanceId) (\s@SignalApplicationInstanceNodeInstancesResponse' {} a -> s {applicationInstanceId = a} :: SignalApplicationInstanceNodeInstancesResponse)

instance
  Prelude.NFData
    SignalApplicationInstanceNodeInstancesResponse
  where
  rnf
    SignalApplicationInstanceNodeInstancesResponse' {..} =
      Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf applicationInstanceId
