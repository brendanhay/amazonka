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
-- Module      : Network.AWS.MigrationHub.NotifyApplicationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the migration state of an application. For a given application
-- identified by the value passed to @ApplicationId@, its status is set or
-- updated by passing one of three values to @Status@:
-- @NOT_STARTED | IN_PROGRESS | COMPLETED@.
module Network.AWS.MigrationHub.NotifyApplicationState
  ( -- * Creating a Request
    NotifyApplicationState (..),
    newNotifyApplicationState,

    -- * Request Lenses
    notifyApplicationState_dryRun,
    notifyApplicationState_updateDateTime,
    notifyApplicationState_applicationId,
    notifyApplicationState_status,

    -- * Destructuring the Response
    NotifyApplicationStateResponse (..),
    newNotifyApplicationStateResponse,

    -- * Response Lenses
    notifyApplicationStateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newNotifyApplicationState' smart constructor.
data NotifyApplicationState = NotifyApplicationState'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the application state changed.
    updateDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The configurationId in Application Discovery Service that uniquely
    -- identifies the grouped application.
    applicationId :: Prelude.Text,
    -- | Status of the application - Not Started, In-Progress, Complete.
    status :: ApplicationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotifyApplicationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'notifyApplicationState_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'updateDateTime', 'notifyApplicationState_updateDateTime' - The timestamp when the application state changed.
--
-- 'applicationId', 'notifyApplicationState_applicationId' - The configurationId in Application Discovery Service that uniquely
-- identifies the grouped application.
--
-- 'status', 'notifyApplicationState_status' - Status of the application - Not Started, In-Progress, Complete.
newNotifyApplicationState ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'status'
  ApplicationStatus ->
  NotifyApplicationState
newNotifyApplicationState pApplicationId_ pStatus_ =
  NotifyApplicationState'
    { dryRun = Prelude.Nothing,
      updateDateTime = Prelude.Nothing,
      applicationId = pApplicationId_,
      status = pStatus_
    }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
notifyApplicationState_dryRun :: Lens.Lens' NotifyApplicationState (Prelude.Maybe Prelude.Bool)
notifyApplicationState_dryRun = Lens.lens (\NotifyApplicationState' {dryRun} -> dryRun) (\s@NotifyApplicationState' {} a -> s {dryRun = a} :: NotifyApplicationState)

-- | The timestamp when the application state changed.
notifyApplicationState_updateDateTime :: Lens.Lens' NotifyApplicationState (Prelude.Maybe Prelude.UTCTime)
notifyApplicationState_updateDateTime = Lens.lens (\NotifyApplicationState' {updateDateTime} -> updateDateTime) (\s@NotifyApplicationState' {} a -> s {updateDateTime = a} :: NotifyApplicationState) Prelude.. Lens.mapping Prelude._Time

-- | The configurationId in Application Discovery Service that uniquely
-- identifies the grouped application.
notifyApplicationState_applicationId :: Lens.Lens' NotifyApplicationState Prelude.Text
notifyApplicationState_applicationId = Lens.lens (\NotifyApplicationState' {applicationId} -> applicationId) (\s@NotifyApplicationState' {} a -> s {applicationId = a} :: NotifyApplicationState)

-- | Status of the application - Not Started, In-Progress, Complete.
notifyApplicationState_status :: Lens.Lens' NotifyApplicationState ApplicationStatus
notifyApplicationState_status = Lens.lens (\NotifyApplicationState' {status} -> status) (\s@NotifyApplicationState' {} a -> s {status = a} :: NotifyApplicationState)

instance Prelude.AWSRequest NotifyApplicationState where
  type
    Rs NotifyApplicationState =
      NotifyApplicationStateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyApplicationStateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyApplicationState

instance Prelude.NFData NotifyApplicationState

instance Prelude.ToHeaders NotifyApplicationState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.NotifyApplicationState" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON NotifyApplicationState where
  toJSON NotifyApplicationState' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DryRun" Prelude..=) Prelude.<$> dryRun,
            ("UpdateDateTime" Prelude..=)
              Prelude.<$> updateDateTime,
            Prelude.Just
              ("ApplicationId" Prelude..= applicationId),
            Prelude.Just ("Status" Prelude..= status)
          ]
      )

instance Prelude.ToPath NotifyApplicationState where
  toPath = Prelude.const "/"

instance Prelude.ToQuery NotifyApplicationState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyApplicationStateResponse' smart constructor.
data NotifyApplicationStateResponse = NotifyApplicationStateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotifyApplicationStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyApplicationStateResponse_httpStatus' - The response's http status code.
newNotifyApplicationStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyApplicationStateResponse
newNotifyApplicationStateResponse pHttpStatus_ =
  NotifyApplicationStateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
notifyApplicationStateResponse_httpStatus :: Lens.Lens' NotifyApplicationStateResponse Prelude.Int
notifyApplicationStateResponse_httpStatus = Lens.lens (\NotifyApplicationStateResponse' {httpStatus} -> httpStatus) (\s@NotifyApplicationStateResponse' {} a -> s {httpStatus = a} :: NotifyApplicationStateResponse)

instance
  Prelude.NFData
    NotifyApplicationStateResponse
