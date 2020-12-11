{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a snapshot schedule that can be associated to a cluster and which overrides the default system backup schedule.
module Network.AWS.Redshift.CreateSnapshotSchedule
  ( -- * Creating a request
    CreateSnapshotSchedule (..),
    mkCreateSnapshotSchedule,

    -- ** Request lenses
    cssNextInvocations,
    cssScheduleDefinitions,
    cssScheduleDescription,
    cssScheduleIdentifier,
    cssDryRun,
    cssTags,

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

-- | /See:/ 'mkCreateSnapshotSchedule' smart constructor.
data CreateSnapshotSchedule = CreateSnapshotSchedule'
  { nextInvocations ::
      Lude.Maybe Lude.Int,
    scheduleDefinitions :: Lude.Maybe [Lude.Text],
    scheduleDescription :: Lude.Maybe Lude.Text,
    scheduleIdentifier :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'dryRun' -
-- * 'nextInvocations' -
-- * 'scheduleDefinitions' - The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)".
-- * 'scheduleDescription' - The description of the snapshot schedule.
-- * 'scheduleIdentifier' - A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
-- * 'tags' - An optional set of tags you can use to search for the schedule.
mkCreateSnapshotSchedule ::
  CreateSnapshotSchedule
mkCreateSnapshotSchedule =
  CreateSnapshotSchedule'
    { nextInvocations = Lude.Nothing,
      scheduleDefinitions = Lude.Nothing,
      scheduleDescription = Lude.Nothing,
      scheduleIdentifier = Lude.Nothing,
      dryRun = Lude.Nothing,
      tags = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssNextInvocations :: Lens.Lens' CreateSnapshotSchedule (Lude.Maybe Lude.Int)
cssNextInvocations = Lens.lens (nextInvocations :: CreateSnapshotSchedule -> Lude.Maybe Lude.Int) (\s a -> s {nextInvocations = a} :: CreateSnapshotSchedule)
{-# DEPRECATED cssNextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead." #-}

-- | The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)".
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleDefinitions :: Lens.Lens' CreateSnapshotSchedule (Lude.Maybe [Lude.Text])
cssScheduleDefinitions = Lens.lens (scheduleDefinitions :: CreateSnapshotSchedule -> Lude.Maybe [Lude.Text]) (\s a -> s {scheduleDefinitions = a} :: CreateSnapshotSchedule)
{-# DEPRECATED cssScheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead." #-}

-- | The description of the snapshot schedule.
--
-- /Note:/ Consider using 'scheduleDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleDescription :: Lens.Lens' CreateSnapshotSchedule (Lude.Maybe Lude.Text)
cssScheduleDescription = Lens.lens (scheduleDescription :: CreateSnapshotSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleDescription = a} :: CreateSnapshotSchedule)
{-# DEPRECATED cssScheduleDescription "Use generic-lens or generic-optics with 'scheduleDescription' instead." #-}

-- | A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleIdentifier :: Lens.Lens' CreateSnapshotSchedule (Lude.Maybe Lude.Text)
cssScheduleIdentifier = Lens.lens (scheduleIdentifier :: CreateSnapshotSchedule -> Lude.Maybe Lude.Text) (\s a -> s {scheduleIdentifier = a} :: CreateSnapshotSchedule)
{-# DEPRECATED cssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

-- |
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDryRun :: Lens.Lens' CreateSnapshotSchedule (Lude.Maybe Lude.Bool)
cssDryRun = Lens.lens (dryRun :: CreateSnapshotSchedule -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSnapshotSchedule)
{-# DEPRECATED cssDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | An optional set of tags you can use to search for the schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTags :: Lens.Lens' CreateSnapshotSchedule (Lude.Maybe [Tag])
cssTags = Lens.lens (tags :: CreateSnapshotSchedule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSnapshotSchedule)
{-# DEPRECATED cssTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateSnapshotSchedule where
  type Rs CreateSnapshotSchedule = SnapshotSchedule
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateSnapshotScheduleResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateSnapshotSchedule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshotSchedule where
  toQuery CreateSnapshotSchedule' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSnapshotSchedule" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "NextInvocations" Lude.=: nextInvocations,
        "ScheduleDefinitions"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "ScheduleDefinition"
                Lude.<$> scheduleDefinitions
            ),
        "ScheduleDescription" Lude.=: scheduleDescription,
        "ScheduleIdentifier" Lude.=: scheduleIdentifier,
        "DryRun" Lude.=: dryRun,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]
