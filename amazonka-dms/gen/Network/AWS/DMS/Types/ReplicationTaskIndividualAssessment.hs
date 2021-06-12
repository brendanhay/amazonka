{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information that describes an individual assessment from a
-- premigration assessment run.
--
-- /See:/ 'newReplicationTaskIndividualAssessment' smart constructor.
data ReplicationTaskIndividualAssessment = ReplicationTaskIndividualAssessment'
  { -- | Individual assessment status.
    --
    -- This status can have one of the following values:
    --
    -- -   @\"cancelled\"@
    --
    -- -   @\"error\"@
    --
    -- -   @\"failed\"@
    --
    -- -   @\"passed\"@
    --
    -- -   @\"pending\"@
    --
    -- -   @\"running\"@
    status :: Core.Maybe Core.Text,
    -- | ARN of the premigration assessment run that is created to run this
    -- individual assessment.
    replicationTaskAssessmentRunArn :: Core.Maybe Core.Text,
    -- | Name of this individual assessment.
    individualAssessmentName :: Core.Maybe Core.Text,
    -- | Date when this individual assessment was started as part of running the
    -- @StartReplicationTaskAssessmentRun@ operation.
    replicationTaskIndividualAssessmentStartDate :: Core.Maybe Core.POSIX,
    -- | Amazon Resource Name (ARN) of this individual assessment.
    replicationTaskIndividualAssessmentArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationTaskIndividualAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'replicationTaskIndividualAssessment_status' - Individual assessment status.
--
-- This status can have one of the following values:
--
-- -   @\"cancelled\"@
--
-- -   @\"error\"@
--
-- -   @\"failed\"@
--
-- -   @\"passed\"@
--
-- -   @\"pending\"@
--
-- -   @\"running\"@
--
-- 'replicationTaskAssessmentRunArn', 'replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn' - ARN of the premigration assessment run that is created to run this
-- individual assessment.
--
-- 'individualAssessmentName', 'replicationTaskIndividualAssessment_individualAssessmentName' - Name of this individual assessment.
--
-- 'replicationTaskIndividualAssessmentStartDate', 'replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate' - Date when this individual assessment was started as part of running the
-- @StartReplicationTaskAssessmentRun@ operation.
--
-- 'replicationTaskIndividualAssessmentArn', 'replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn' - Amazon Resource Name (ARN) of this individual assessment.
newReplicationTaskIndividualAssessment ::
  ReplicationTaskIndividualAssessment
newReplicationTaskIndividualAssessment =
  ReplicationTaskIndividualAssessment'
    { status =
        Core.Nothing,
      replicationTaskAssessmentRunArn =
        Core.Nothing,
      individualAssessmentName =
        Core.Nothing,
      replicationTaskIndividualAssessmentStartDate =
        Core.Nothing,
      replicationTaskIndividualAssessmentArn =
        Core.Nothing
    }

-- | Individual assessment status.
--
-- This status can have one of the following values:
--
-- -   @\"cancelled\"@
--
-- -   @\"error\"@
--
-- -   @\"failed\"@
--
-- -   @\"passed\"@
--
-- -   @\"pending\"@
--
-- -   @\"running\"@
replicationTaskIndividualAssessment_status :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
replicationTaskIndividualAssessment_status = Lens.lens (\ReplicationTaskIndividualAssessment' {status} -> status) (\s@ReplicationTaskIndividualAssessment' {} a -> s {status = a} :: ReplicationTaskIndividualAssessment)

-- | ARN of the premigration assessment run that is created to run this
-- individual assessment.
replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn = Lens.lens (\ReplicationTaskIndividualAssessment' {replicationTaskAssessmentRunArn} -> replicationTaskAssessmentRunArn) (\s@ReplicationTaskIndividualAssessment' {} a -> s {replicationTaskAssessmentRunArn = a} :: ReplicationTaskIndividualAssessment)

-- | Name of this individual assessment.
replicationTaskIndividualAssessment_individualAssessmentName :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
replicationTaskIndividualAssessment_individualAssessmentName = Lens.lens (\ReplicationTaskIndividualAssessment' {individualAssessmentName} -> individualAssessmentName) (\s@ReplicationTaskIndividualAssessment' {} a -> s {individualAssessmentName = a} :: ReplicationTaskIndividualAssessment)

-- | Date when this individual assessment was started as part of running the
-- @StartReplicationTaskAssessmentRun@ operation.
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.UTCTime)
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate = Lens.lens (\ReplicationTaskIndividualAssessment' {replicationTaskIndividualAssessmentStartDate} -> replicationTaskIndividualAssessmentStartDate) (\s@ReplicationTaskIndividualAssessment' {} a -> s {replicationTaskIndividualAssessmentStartDate = a} :: ReplicationTaskIndividualAssessment) Core.. Lens.mapping Core._Time

-- | Amazon Resource Name (ARN) of this individual assessment.
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn = Lens.lens (\ReplicationTaskIndividualAssessment' {replicationTaskIndividualAssessmentArn} -> replicationTaskIndividualAssessmentArn) (\s@ReplicationTaskIndividualAssessment' {} a -> s {replicationTaskIndividualAssessmentArn = a} :: ReplicationTaskIndividualAssessment)

instance
  Core.FromJSON
    ReplicationTaskIndividualAssessment
  where
  parseJSON =
    Core.withObject
      "ReplicationTaskIndividualAssessment"
      ( \x ->
          ReplicationTaskIndividualAssessment'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "ReplicationTaskAssessmentRunArn")
            Core.<*> (x Core..:? "IndividualAssessmentName")
            Core.<*> ( x
                         Core..:? "ReplicationTaskIndividualAssessmentStartDate"
                     )
            Core.<*> ( x
                         Core..:? "ReplicationTaskIndividualAssessmentArn"
                     )
      )

instance
  Core.Hashable
    ReplicationTaskIndividualAssessment

instance
  Core.NFData
    ReplicationTaskIndividualAssessment
