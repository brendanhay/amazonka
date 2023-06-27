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
-- Module      : Amazonka.CloudFormation.SignalResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to the specified resource with a success or failure
-- status. You can use the @SignalResource@ operation in conjunction with a
-- creation policy or update policy. CloudFormation doesn\'t proceed with a
-- stack creation or update until resources receive the required number of
-- signals or the timeout period is exceeded. The @SignalResource@
-- operation is useful in cases where you want to send signals from
-- anywhere other than an Amazon EC2 instance.
module Amazonka.CloudFormation.SignalResource
  ( -- * Creating a Request
    SignalResource (..),
    newSignalResource,

    -- * Request Lenses
    signalResource_stackName,
    signalResource_logicalResourceId,
    signalResource_uniqueId,
    signalResource_status,

    -- * Destructuring the Response
    SignalResourceResponse (..),
    newSignalResourceResponse,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the SignalResource action.
--
-- /See:/ 'newSignalResource' smart constructor.
data SignalResource = SignalResource'
  { -- | The stack name or unique stack ID that includes the resource that you
    -- want to signal.
    stackName :: Prelude.Text,
    -- | The logical ID of the resource that you want to signal. The logical ID
    -- is the name of the resource that given in the template.
    logicalResourceId :: Prelude.Text,
    -- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto
    -- Scaling groups, specify the instance ID that you are signaling as the
    -- unique ID. If you send multiple signals to a single resource (such as
    -- signaling a wait condition), each signal requires a different unique ID.
    uniqueId :: Prelude.Text,
    -- | The status of the signal, which is either success or failure. A failure
    -- signal causes CloudFormation to immediately fail the stack creation or
    -- update.
    status :: ResourceSignalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'signalResource_stackName' - The stack name or unique stack ID that includes the resource that you
-- want to signal.
--
-- 'logicalResourceId', 'signalResource_logicalResourceId' - The logical ID of the resource that you want to signal. The logical ID
-- is the name of the resource that given in the template.
--
-- 'uniqueId', 'signalResource_uniqueId' - A unique ID of the signal. When you signal Amazon EC2 instances or Auto
-- Scaling groups, specify the instance ID that you are signaling as the
-- unique ID. If you send multiple signals to a single resource (such as
-- signaling a wait condition), each signal requires a different unique ID.
--
-- 'status', 'signalResource_status' - The status of the signal, which is either success or failure. A failure
-- signal causes CloudFormation to immediately fail the stack creation or
-- update.
newSignalResource ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'logicalResourceId'
  Prelude.Text ->
  -- | 'uniqueId'
  Prelude.Text ->
  -- | 'status'
  ResourceSignalStatus ->
  SignalResource
newSignalResource
  pStackName_
  pLogicalResourceId_
  pUniqueId_
  pStatus_ =
    SignalResource'
      { stackName = pStackName_,
        logicalResourceId = pLogicalResourceId_,
        uniqueId = pUniqueId_,
        status = pStatus_
      }

-- | The stack name or unique stack ID that includes the resource that you
-- want to signal.
signalResource_stackName :: Lens.Lens' SignalResource Prelude.Text
signalResource_stackName = Lens.lens (\SignalResource' {stackName} -> stackName) (\s@SignalResource' {} a -> s {stackName = a} :: SignalResource)

-- | The logical ID of the resource that you want to signal. The logical ID
-- is the name of the resource that given in the template.
signalResource_logicalResourceId :: Lens.Lens' SignalResource Prelude.Text
signalResource_logicalResourceId = Lens.lens (\SignalResource' {logicalResourceId} -> logicalResourceId) (\s@SignalResource' {} a -> s {logicalResourceId = a} :: SignalResource)

-- | A unique ID of the signal. When you signal Amazon EC2 instances or Auto
-- Scaling groups, specify the instance ID that you are signaling as the
-- unique ID. If you send multiple signals to a single resource (such as
-- signaling a wait condition), each signal requires a different unique ID.
signalResource_uniqueId :: Lens.Lens' SignalResource Prelude.Text
signalResource_uniqueId = Lens.lens (\SignalResource' {uniqueId} -> uniqueId) (\s@SignalResource' {} a -> s {uniqueId = a} :: SignalResource)

-- | The status of the signal, which is either success or failure. A failure
-- signal causes CloudFormation to immediately fail the stack creation or
-- update.
signalResource_status :: Lens.Lens' SignalResource ResourceSignalStatus
signalResource_status = Lens.lens (\SignalResource' {status} -> status) (\s@SignalResource' {} a -> s {status = a} :: SignalResource)

instance Core.AWSRequest SignalResource where
  type
    AWSResponse SignalResource =
      SignalResourceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull SignalResourceResponse'

instance Prelude.Hashable SignalResource where
  hashWithSalt _salt SignalResource' {..} =
    _salt
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` uniqueId
      `Prelude.hashWithSalt` status

instance Prelude.NFData SignalResource where
  rnf SignalResource' {..} =
    Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf uniqueId
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders SignalResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SignalResource where
  toPath = Prelude.const "/"

instance Data.ToQuery SignalResource where
  toQuery SignalResource' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SignalResource" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Data.=: stackName,
        "LogicalResourceId" Data.=: logicalResourceId,
        "UniqueId" Data.=: uniqueId,
        "Status" Data.=: status
      ]

-- | /See:/ 'newSignalResourceResponse' smart constructor.
data SignalResourceResponse = SignalResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignalResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSignalResourceResponse ::
  SignalResourceResponse
newSignalResourceResponse = SignalResourceResponse'

instance Prelude.NFData SignalResourceResponse where
  rnf _ = ()
