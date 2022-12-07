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
-- Module      : Amazonka.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the actions for the specified alarms.
module Amazonka.CloudWatch.EnableAlarmActions
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

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableAlarmActions' smart constructor.
data EnableAlarmActions = EnableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
enableAlarmActions_alarmNames = Lens.lens (\EnableAlarmActions' {alarmNames} -> alarmNames) (\s@EnableAlarmActions' {} a -> s {alarmNames = a} :: EnableAlarmActions) Prelude.. Lens.coerced

instance Core.AWSRequest EnableAlarmActions where
  type
    AWSResponse EnableAlarmActions =
      EnableAlarmActionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull EnableAlarmActionsResponse'

instance Prelude.Hashable EnableAlarmActions where
  hashWithSalt _salt EnableAlarmActions' {..} =
    _salt `Prelude.hashWithSalt` alarmNames

instance Prelude.NFData EnableAlarmActions where
  rnf EnableAlarmActions' {..} = Prelude.rnf alarmNames

instance Data.ToHeaders EnableAlarmActions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableAlarmActions where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableAlarmActions where
  toQuery EnableAlarmActions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableAlarmActions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "AlarmNames"
          Data.=: Data.toQueryList "member" alarmNames
      ]

-- | /See:/ 'newEnableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse = EnableAlarmActionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableAlarmActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableAlarmActionsResponse ::
  EnableAlarmActionsResponse
newEnableAlarmActionsResponse =
  EnableAlarmActionsResponse'

instance Prelude.NFData EnableAlarmActionsResponse where
  rnf _ = ()
