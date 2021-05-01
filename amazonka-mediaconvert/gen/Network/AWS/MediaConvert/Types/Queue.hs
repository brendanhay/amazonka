{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.Queue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Queue where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.PricingPlan
import Network.AWS.MediaConvert.Types.QueueStatus
import Network.AWS.MediaConvert.Types.ReservationPlan
import Network.AWS.MediaConvert.Types.Type
import qualified Network.AWS.Prelude as Prelude

-- | You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
--
-- /See:/ 'newQueue' smart constructor.
data Queue = Queue'
  { -- | Queues can be ACTIVE or PAUSED. If you pause a queue, the service won\'t
    -- begin processing jobs in that queue. Jobs that are running when you
    -- pause the queue continue to run until they finish or result in an error.
    status :: Prelude.Maybe QueueStatus,
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp in epoch seconds for when you created the queue.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The timestamp in epoch seconds for when you most recently updated the
    -- queue.
    lastUpdated :: Prelude.Maybe Prelude.POSIX,
    -- | The estimated number of jobs with a SUBMITTED status.
    submittedJobsCount :: Prelude.Maybe Prelude.Int,
    -- | An optional description that you create for each queue.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the pricing plan for the queue is on-demand or
    -- reserved. For on-demand, you pay per minute, billed in increments of .01
    -- minute. For reserved, you pay for the transcoding capacity of the entire
    -- queue, regardless of how much or how little you use it. Reserved pricing
    -- requires a 12-month commitment.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Details about the pricing plan for your reserved queue. Required for
    -- reserved queues and not applicable to on-demand queues.
    reservationPlan :: Prelude.Maybe ReservationPlan,
    -- | Specifies whether this on-demand queue is system or custom. System
    -- queues are built in. You can\'t modify or delete system queues. You can
    -- create and modify custom queues.
    type' :: Prelude.Maybe Type,
    -- | The estimated number of jobs with a PROGRESSING status.
    progressingJobsCount :: Prelude.Maybe Prelude.Int,
    -- | A name that you create for each queue. Each name must be unique within
    -- your account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Queue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'queue_status' - Queues can be ACTIVE or PAUSED. If you pause a queue, the service won\'t
-- begin processing jobs in that queue. Jobs that are running when you
-- pause the queue continue to run until they finish or result in an error.
--
-- 'arn', 'queue_arn' - An identifier for this resource that is unique within all of AWS.
--
-- 'createdAt', 'queue_createdAt' - The timestamp in epoch seconds for when you created the queue.
--
-- 'lastUpdated', 'queue_lastUpdated' - The timestamp in epoch seconds for when you most recently updated the
-- queue.
--
-- 'submittedJobsCount', 'queue_submittedJobsCount' - The estimated number of jobs with a SUBMITTED status.
--
-- 'description', 'queue_description' - An optional description that you create for each queue.
--
-- 'pricingPlan', 'queue_pricingPlan' - Specifies whether the pricing plan for the queue is on-demand or
-- reserved. For on-demand, you pay per minute, billed in increments of .01
-- minute. For reserved, you pay for the transcoding capacity of the entire
-- queue, regardless of how much or how little you use it. Reserved pricing
-- requires a 12-month commitment.
--
-- 'reservationPlan', 'queue_reservationPlan' - Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
--
-- 'type'', 'queue_type' - Specifies whether this on-demand queue is system or custom. System
-- queues are built in. You can\'t modify or delete system queues. You can
-- create and modify custom queues.
--
-- 'progressingJobsCount', 'queue_progressingJobsCount' - The estimated number of jobs with a PROGRESSING status.
--
-- 'name', 'queue_name' - A name that you create for each queue. Each name must be unique within
-- your account.
newQueue ::
  -- | 'name'
  Prelude.Text ->
  Queue
newQueue pName_ =
  Queue'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      submittedJobsCount = Prelude.Nothing,
      description = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      reservationPlan = Prelude.Nothing,
      type' = Prelude.Nothing,
      progressingJobsCount = Prelude.Nothing,
      name = pName_
    }

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, the service won\'t
-- begin processing jobs in that queue. Jobs that are running when you
-- pause the queue continue to run until they finish or result in an error.
queue_status :: Lens.Lens' Queue (Prelude.Maybe QueueStatus)
queue_status = Lens.lens (\Queue' {status} -> status) (\s@Queue' {} a -> s {status = a} :: Queue)

-- | An identifier for this resource that is unique within all of AWS.
queue_arn :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_arn = Lens.lens (\Queue' {arn} -> arn) (\s@Queue' {} a -> s {arn = a} :: Queue)

-- | The timestamp in epoch seconds for when you created the queue.
queue_createdAt :: Lens.Lens' Queue (Prelude.Maybe Prelude.UTCTime)
queue_createdAt = Lens.lens (\Queue' {createdAt} -> createdAt) (\s@Queue' {} a -> s {createdAt = a} :: Queue) Prelude.. Lens.mapping Prelude._Time

-- | The timestamp in epoch seconds for when you most recently updated the
-- queue.
queue_lastUpdated :: Lens.Lens' Queue (Prelude.Maybe Prelude.UTCTime)
queue_lastUpdated = Lens.lens (\Queue' {lastUpdated} -> lastUpdated) (\s@Queue' {} a -> s {lastUpdated = a} :: Queue) Prelude.. Lens.mapping Prelude._Time

-- | The estimated number of jobs with a SUBMITTED status.
queue_submittedJobsCount :: Lens.Lens' Queue (Prelude.Maybe Prelude.Int)
queue_submittedJobsCount = Lens.lens (\Queue' {submittedJobsCount} -> submittedJobsCount) (\s@Queue' {} a -> s {submittedJobsCount = a} :: Queue)

-- | An optional description that you create for each queue.
queue_description :: Lens.Lens' Queue (Prelude.Maybe Prelude.Text)
queue_description = Lens.lens (\Queue' {description} -> description) (\s@Queue' {} a -> s {description = a} :: Queue)

-- | Specifies whether the pricing plan for the queue is on-demand or
-- reserved. For on-demand, you pay per minute, billed in increments of .01
-- minute. For reserved, you pay for the transcoding capacity of the entire
-- queue, regardless of how much or how little you use it. Reserved pricing
-- requires a 12-month commitment.
queue_pricingPlan :: Lens.Lens' Queue (Prelude.Maybe PricingPlan)
queue_pricingPlan = Lens.lens (\Queue' {pricingPlan} -> pricingPlan) (\s@Queue' {} a -> s {pricingPlan = a} :: Queue)

-- | Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
queue_reservationPlan :: Lens.Lens' Queue (Prelude.Maybe ReservationPlan)
queue_reservationPlan = Lens.lens (\Queue' {reservationPlan} -> reservationPlan) (\s@Queue' {} a -> s {reservationPlan = a} :: Queue)

-- | Specifies whether this on-demand queue is system or custom. System
-- queues are built in. You can\'t modify or delete system queues. You can
-- create and modify custom queues.
queue_type :: Lens.Lens' Queue (Prelude.Maybe Type)
queue_type = Lens.lens (\Queue' {type'} -> type') (\s@Queue' {} a -> s {type' = a} :: Queue)

-- | The estimated number of jobs with a PROGRESSING status.
queue_progressingJobsCount :: Lens.Lens' Queue (Prelude.Maybe Prelude.Int)
queue_progressingJobsCount = Lens.lens (\Queue' {progressingJobsCount} -> progressingJobsCount) (\s@Queue' {} a -> s {progressingJobsCount = a} :: Queue)

-- | A name that you create for each queue. Each name must be unique within
-- your account.
queue_name :: Lens.Lens' Queue Prelude.Text
queue_name = Lens.lens (\Queue' {name} -> name) (\s@Queue' {} a -> s {name = a} :: Queue)

instance Prelude.FromJSON Queue where
  parseJSON =
    Prelude.withObject
      "Queue"
      ( \x ->
          Queue'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "lastUpdated")
            Prelude.<*> (x Prelude..:? "submittedJobsCount")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "pricingPlan")
            Prelude.<*> (x Prelude..:? "reservationPlan")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "progressingJobsCount")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable Queue

instance Prelude.NFData Queue
