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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableAlarmActions' smart constructor.
data EnableAlarmActions = EnableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  EnableAlarmActions' {alarmNames = Prelude.mempty}

-- | The names of the alarms.
enableAlarmActions_alarmNames :: Lens.Lens' EnableAlarmActions [Prelude.Text]
enableAlarmActions_alarmNames = Lens.lens (\EnableAlarmActions' {alarmNames} -> alarmNames) (\s@EnableAlarmActions' {} a -> s {alarmNames = a} :: EnableAlarmActions) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest EnableAlarmActions where
  type
    Rs EnableAlarmActions =
      EnableAlarmActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull EnableAlarmActionsResponse'

instance Prelude.Hashable EnableAlarmActions

instance Prelude.NFData EnableAlarmActions

instance Prelude.ToHeaders EnableAlarmActions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath EnableAlarmActions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableAlarmActions where
  toQuery EnableAlarmActions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("EnableAlarmActions" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "AlarmNames"
          Prelude.=: Prelude.toQueryList "member" alarmNames
      ]

-- | /See:/ 'newEnableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse = EnableAlarmActionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableAlarmActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAlarmActionsResponse ::
  EnableAlarmActionsResponse
newEnableAlarmActionsResponse =
  EnableAlarmActionsResponse'

instance Prelude.NFData EnableAlarmActionsResponse
