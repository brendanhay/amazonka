{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule. Any schedule associated with a cluster is modified asynchronously.
module Network.AWS.Redshift.ModifySnapshotSchedule
  ( -- * Creating a request
    ModifySnapshotSchedule (..),
    mkModifySnapshotSchedule,

    -- ** Request lenses
    mssScheduleIdentifier,
    mssScheduleDefinitions,

    -- * Destructuring the response
    SnapshotSchedule (..),
    mkSnapshotSchedule,

    -- ** Response lenses
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssAssociatedClusterCount,
    ssTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifySnapshotSchedule' smart constructor.
data ModifySnapshotSchedule = ModifySnapshotSchedule'
  { scheduleIdentifier ::
      Lude.Text,
    scheduleDefinitions :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'scheduleDefinitions' - An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
-- * 'scheduleIdentifier' - A unique alphanumeric identifier of the schedule to modify.
mkModifySnapshotSchedule ::
  -- | 'scheduleIdentifier'
  Lude.Text ->
  ModifySnapshotSchedule
mkModifySnapshotSchedule pScheduleIdentifier_ =
  ModifySnapshotSchedule'
    { scheduleIdentifier =
        pScheduleIdentifier_,
      scheduleDefinitions = Lude.mempty
    }

-- | A unique alphanumeric identifier of the schedule to modify.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScheduleIdentifier :: Lens.Lens' ModifySnapshotSchedule Lude.Text
mssScheduleIdentifier = Lens.lens (scheduleIdentifier :: ModifySnapshotSchedule -> Lude.Text) (\s a -> s {scheduleIdentifier = a} :: ModifySnapshotSchedule)
{-# DEPRECATED mssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

-- | An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScheduleDefinitions :: Lens.Lens' ModifySnapshotSchedule [Lude.Text]
mssScheduleDefinitions = Lens.lens (scheduleDefinitions :: ModifySnapshotSchedule -> [Lude.Text]) (\s a -> s {scheduleDefinitions = a} :: ModifySnapshotSchedule)
{-# DEPRECATED mssScheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead." #-}

instance Lude.AWSRequest ModifySnapshotSchedule where
  type Rs ModifySnapshotSchedule = SnapshotSchedule
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifySnapshotScheduleResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifySnapshotSchedule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifySnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifySnapshotSchedule where
  toQuery ModifySnapshotSchedule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifySnapshotSchedule" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ScheduleIdentifier" Lude.=: scheduleIdentifier,
        "ScheduleDefinitions"
          Lude.=: Lude.toQueryList "ScheduleDefinition" scheduleDefinitions
      ]
