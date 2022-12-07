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
-- Module      : Amazonka.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alarms. You can delete up to 100 alarms in one
-- operation. However, this total can include no more than one composite
-- alarm. For example, you could delete 99 metric alarms and one composite
-- alarms with one operation, but you can\'t delete two composite alarms
-- with one operation.
--
-- In the event of an error, no alarms are deleted.
--
-- It is possible to create a loop or cycle of composite alarms, where
-- composite alarm A depends on composite alarm B, and composite alarm B
-- also depends on composite alarm A. In this scenario, you can\'t delete
-- any composite alarm that is part of the cycle because there is always
-- still a composite alarm that depends on that alarm that you want to
-- delete.
--
-- To get out of such a situation, you must break the cycle by changing the
-- rule of one of the composite alarms in the cycle to remove a dependency
-- that creates the cycle. The simplest change to make to break a cycle is
-- to change the @AlarmRule@ of one of the alarms to @False@.
--
-- Additionally, the evaluation of composite alarms stops if CloudWatch
-- detects a cycle in the evaluation path.
module Amazonka.CloudWatch.DeleteAlarms
  ( -- * Creating a Request
    DeleteAlarms (..),
    newDeleteAlarms,

    -- * Request Lenses
    deleteAlarms_alarmNames,

    -- * Destructuring the Response
    DeleteAlarmsResponse (..),
    newDeleteAlarmsResponse,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlarms' smart constructor.
data DeleteAlarms = DeleteAlarms'
  { -- | The alarms to be deleted.
    alarmNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlarms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmNames', 'deleteAlarms_alarmNames' - The alarms to be deleted.
newDeleteAlarms ::
  DeleteAlarms
newDeleteAlarms =
  DeleteAlarms' {alarmNames = Prelude.mempty}

-- | The alarms to be deleted.
deleteAlarms_alarmNames :: Lens.Lens' DeleteAlarms [Prelude.Text]
deleteAlarms_alarmNames = Lens.lens (\DeleteAlarms' {alarmNames} -> alarmNames) (\s@DeleteAlarms' {} a -> s {alarmNames = a} :: DeleteAlarms) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteAlarms where
  type AWSResponse DeleteAlarms = DeleteAlarmsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteAlarmsResponse'

instance Prelude.Hashable DeleteAlarms where
  hashWithSalt _salt DeleteAlarms' {..} =
    _salt `Prelude.hashWithSalt` alarmNames

instance Prelude.NFData DeleteAlarms where
  rnf DeleteAlarms' {..} = Prelude.rnf alarmNames

instance Data.ToHeaders DeleteAlarms where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAlarms where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAlarms where
  toQuery DeleteAlarms' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteAlarms" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "AlarmNames"
          Data.=: Data.toQueryList "member" alarmNames
      ]

-- | /See:/ 'newDeleteAlarmsResponse' smart constructor.
data DeleteAlarmsResponse = DeleteAlarmsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlarmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAlarmsResponse ::
  DeleteAlarmsResponse
newDeleteAlarmsResponse = DeleteAlarmsResponse'

instance Prelude.NFData DeleteAlarmsResponse where
  rnf _ = ()
