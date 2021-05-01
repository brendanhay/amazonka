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
-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the actions for the specified alarms. When an alarm\'s actions
-- are disabled, the alarm actions do not execute when the alarm state
-- changes.
module Network.AWS.CloudWatch.DisableAlarmActions
  ( -- * Creating a Request
    DisableAlarmActions (..),
    newDisableAlarmActions,

    -- * Request Lenses
    disableAlarmActions_alarmNames,

    -- * Destructuring the Response
    DisableAlarmActionsResponse (..),
    newDisableAlarmActionsResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableAlarmActions' smart constructor.
data DisableAlarmActions = DisableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableAlarmActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmNames', 'disableAlarmActions_alarmNames' - The names of the alarms.
newDisableAlarmActions ::
  DisableAlarmActions
newDisableAlarmActions =
  DisableAlarmActions' {alarmNames = Prelude.mempty}

-- | The names of the alarms.
disableAlarmActions_alarmNames :: Lens.Lens' DisableAlarmActions [Prelude.Text]
disableAlarmActions_alarmNames = Lens.lens (\DisableAlarmActions' {alarmNames} -> alarmNames) (\s@DisableAlarmActions' {} a -> s {alarmNames = a} :: DisableAlarmActions) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DisableAlarmActions where
  type
    Rs DisableAlarmActions =
      DisableAlarmActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DisableAlarmActionsResponse'

instance Prelude.Hashable DisableAlarmActions

instance Prelude.NFData DisableAlarmActions

instance Prelude.ToHeaders DisableAlarmActions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisableAlarmActions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableAlarmActions where
  toQuery DisableAlarmActions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DisableAlarmActions" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "AlarmNames"
          Prelude.=: Prelude.toQueryList "member" alarmNames
      ]

-- | /See:/ 'newDisableAlarmActionsResponse' smart constructor.
data DisableAlarmActionsResponse = DisableAlarmActionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableAlarmActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableAlarmActionsResponse ::
  DisableAlarmActionsResponse
newDisableAlarmActionsResponse =
  DisableAlarmActionsResponse'

instance Prelude.NFData DisableAlarmActionsResponse
