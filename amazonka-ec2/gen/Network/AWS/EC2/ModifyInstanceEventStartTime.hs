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
-- Module      : Network.AWS.EC2.ModifyInstanceEventStartTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the start time for a scheduled Amazon EC2 instance event.
module Network.AWS.EC2.ModifyInstanceEventStartTime
  ( -- * Creating a Request
    ModifyInstanceEventStartTime (..),
    newModifyInstanceEventStartTime,

    -- * Request Lenses
    modifyInstanceEventStartTime_dryRun,
    modifyInstanceEventStartTime_instanceId,
    modifyInstanceEventStartTime_instanceEventId,
    modifyInstanceEventStartTime_notBefore,

    -- * Destructuring the Response
    ModifyInstanceEventStartTimeResponse (..),
    newModifyInstanceEventStartTimeResponse,

    -- * Response Lenses
    modifyInstanceEventStartTimeResponse_event,
    modifyInstanceEventStartTimeResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceEventStartTime' smart constructor.
data ModifyInstanceEventStartTime = ModifyInstanceEventStartTime'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance with the scheduled event.
    instanceId :: Prelude.Text,
    -- | The ID of the event whose date and time you are modifying.
    instanceEventId :: Prelude.Text,
    -- | The new date and time when the event will take place.
    notBefore :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceEventStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyInstanceEventStartTime_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'modifyInstanceEventStartTime_instanceId' - The ID of the instance with the scheduled event.
--
-- 'instanceEventId', 'modifyInstanceEventStartTime_instanceEventId' - The ID of the event whose date and time you are modifying.
--
-- 'notBefore', 'modifyInstanceEventStartTime_notBefore' - The new date and time when the event will take place.
newModifyInstanceEventStartTime ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'instanceEventId'
  Prelude.Text ->
  -- | 'notBefore'
  Prelude.UTCTime ->
  ModifyInstanceEventStartTime
newModifyInstanceEventStartTime
  pInstanceId_
  pInstanceEventId_
  pNotBefore_ =
    ModifyInstanceEventStartTime'
      { dryRun =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        instanceEventId = pInstanceEventId_,
        notBefore = Prelude._Time Lens.# pNotBefore_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceEventStartTime_dryRun :: Lens.Lens' ModifyInstanceEventStartTime (Prelude.Maybe Prelude.Bool)
modifyInstanceEventStartTime_dryRun = Lens.lens (\ModifyInstanceEventStartTime' {dryRun} -> dryRun) (\s@ModifyInstanceEventStartTime' {} a -> s {dryRun = a} :: ModifyInstanceEventStartTime)

-- | The ID of the instance with the scheduled event.
modifyInstanceEventStartTime_instanceId :: Lens.Lens' ModifyInstanceEventStartTime Prelude.Text
modifyInstanceEventStartTime_instanceId = Lens.lens (\ModifyInstanceEventStartTime' {instanceId} -> instanceId) (\s@ModifyInstanceEventStartTime' {} a -> s {instanceId = a} :: ModifyInstanceEventStartTime)

-- | The ID of the event whose date and time you are modifying.
modifyInstanceEventStartTime_instanceEventId :: Lens.Lens' ModifyInstanceEventStartTime Prelude.Text
modifyInstanceEventStartTime_instanceEventId = Lens.lens (\ModifyInstanceEventStartTime' {instanceEventId} -> instanceEventId) (\s@ModifyInstanceEventStartTime' {} a -> s {instanceEventId = a} :: ModifyInstanceEventStartTime)

-- | The new date and time when the event will take place.
modifyInstanceEventStartTime_notBefore :: Lens.Lens' ModifyInstanceEventStartTime Prelude.UTCTime
modifyInstanceEventStartTime_notBefore = Lens.lens (\ModifyInstanceEventStartTime' {notBefore} -> notBefore) (\s@ModifyInstanceEventStartTime' {} a -> s {notBefore = a} :: ModifyInstanceEventStartTime) Prelude.. Prelude._Time

instance
  Prelude.AWSRequest
    ModifyInstanceEventStartTime
  where
  type
    Rs ModifyInstanceEventStartTime =
      ModifyInstanceEventStartTimeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceEventStartTimeResponse'
            Prelude.<$> (x Prelude..@? "event")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceEventStartTime

instance Prelude.NFData ModifyInstanceEventStartTime

instance
  Prelude.ToHeaders
    ModifyInstanceEventStartTime
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyInstanceEventStartTime where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyInstanceEventStartTime where
  toQuery ModifyInstanceEventStartTime' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyInstanceEventStartTime" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "InstanceId" Prelude.=: instanceId,
        "InstanceEventId" Prelude.=: instanceEventId,
        "NotBefore" Prelude.=: notBefore
      ]

-- | /See:/ 'newModifyInstanceEventStartTimeResponse' smart constructor.
data ModifyInstanceEventStartTimeResponse = ModifyInstanceEventStartTimeResponse'
  { event :: Prelude.Maybe InstanceStatusEvent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceEventStartTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'modifyInstanceEventStartTimeResponse_event' - Undocumented member.
--
-- 'httpStatus', 'modifyInstanceEventStartTimeResponse_httpStatus' - The response's http status code.
newModifyInstanceEventStartTimeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceEventStartTimeResponse
newModifyInstanceEventStartTimeResponse pHttpStatus_ =
  ModifyInstanceEventStartTimeResponse'
    { event =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyInstanceEventStartTimeResponse_event :: Lens.Lens' ModifyInstanceEventStartTimeResponse (Prelude.Maybe InstanceStatusEvent)
modifyInstanceEventStartTimeResponse_event = Lens.lens (\ModifyInstanceEventStartTimeResponse' {event} -> event) (\s@ModifyInstanceEventStartTimeResponse' {} a -> s {event = a} :: ModifyInstanceEventStartTimeResponse)

-- | The response's http status code.
modifyInstanceEventStartTimeResponse_httpStatus :: Lens.Lens' ModifyInstanceEventStartTimeResponse Prelude.Int
modifyInstanceEventStartTimeResponse_httpStatus = Lens.lens (\ModifyInstanceEventStartTimeResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceEventStartTimeResponse' {} a -> s {httpStatus = a} :: ModifyInstanceEventStartTimeResponse)

instance
  Prelude.NFData
    ModifyInstanceEventStartTimeResponse
