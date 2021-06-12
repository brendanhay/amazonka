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
-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the actions for the specified alarms.
module Network.AWS.CloudWatch.EnableAlarmActions
  ( -- * Creating a Request
    EnableAlarmActions (..),
    newEnableAlarmActions,

    -- * Request Lenses
    enableAlarmActions_alarmNames,

    -- * Destructuring the Response
    EnableAlarmActionsResponse (..),
    newEnableAlarmActionsResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableAlarmActions' smart constructor.
data EnableAlarmActions = EnableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAlarmActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmNames', 'enableAlarmActions_alarmNames' - The names of the alarms.
newEnableAlarmActions ::
  EnableAlarmActions
newEnableAlarmActions =
  EnableAlarmActions' {alarmNames = Core.mempty}

-- | The names of the alarms.
enableAlarmActions_alarmNames :: Lens.Lens' EnableAlarmActions [Core.Text]
enableAlarmActions_alarmNames = Lens.lens (\EnableAlarmActions' {alarmNames} -> alarmNames) (\s@EnableAlarmActions' {} a -> s {alarmNames = a} :: EnableAlarmActions) Core.. Lens._Coerce

instance Core.AWSRequest EnableAlarmActions where
  type
    AWSResponse EnableAlarmActions =
      EnableAlarmActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull EnableAlarmActionsResponse'

instance Core.Hashable EnableAlarmActions

instance Core.NFData EnableAlarmActions

instance Core.ToHeaders EnableAlarmActions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EnableAlarmActions where
  toPath = Core.const "/"

instance Core.ToQuery EnableAlarmActions where
  toQuery EnableAlarmActions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("EnableAlarmActions" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "AlarmNames"
          Core.=: Core.toQueryList "member" alarmNames
      ]

-- | /See:/ 'newEnableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse = EnableAlarmActionsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAlarmActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAlarmActionsResponse ::
  EnableAlarmActionsResponse
newEnableAlarmActionsResponse =
  EnableAlarmActionsResponse'

instance Core.NFData EnableAlarmActionsResponse
