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
-- Module      : Amazonka.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the actions for the specified alarms. When an alarm\'s actions
-- are disabled, the alarm actions do not execute when the alarm state
-- changes.
module Amazonka.CloudWatch.DisableAlarmActions
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

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableAlarmActions' smart constructor.
data DisableAlarmActions = DisableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
disableAlarmActions_alarmNames = Lens.lens (\DisableAlarmActions' {alarmNames} -> alarmNames) (\s@DisableAlarmActions' {} a -> s {alarmNames = a} :: DisableAlarmActions) Prelude.. Lens.coerced

instance Core.AWSRequest DisableAlarmActions where
  type
    AWSResponse DisableAlarmActions =
      DisableAlarmActionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DisableAlarmActionsResponse'

instance Prelude.Hashable DisableAlarmActions where
  hashWithSalt _salt DisableAlarmActions' {..} =
    _salt `Prelude.hashWithSalt` alarmNames

instance Prelude.NFData DisableAlarmActions where
  rnf DisableAlarmActions' {..} = Prelude.rnf alarmNames

instance Core.ToHeaders DisableAlarmActions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisableAlarmActions where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableAlarmActions where
  toQuery DisableAlarmActions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DisableAlarmActions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "AlarmNames"
          Core.=: Core.toQueryList "member" alarmNames
      ]

-- | /See:/ 'newDisableAlarmActionsResponse' smart constructor.
data DisableAlarmActionsResponse = DisableAlarmActionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableAlarmActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableAlarmActionsResponse ::
  DisableAlarmActionsResponse
newDisableAlarmActionsResponse =
  DisableAlarmActionsResponse'

instance Prelude.NFData DisableAlarmActionsResponse where
  rnf _ = ()
