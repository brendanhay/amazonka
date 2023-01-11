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
-- Module      : Amazonka.Evidently.Types.Segment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Segment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about one audience /segment/. You
-- can use segments in your experiments and launches to narrow the user
-- sessions used for experiment or launch to only the user sessions that
-- match one or more criteria.
--
-- /See:/ 'newSegment' smart constructor.
data Segment = Segment'
  { -- | The customer-created description for this segment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of experiments that this segment is used in. This count
    -- includes all current experiments, not just those that are currently
    -- running.
    experimentCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of launches that this segment is used in. This count includes
    -- all current launches, not just those that are currently running.
    launchCount :: Prelude.Maybe Prelude.Integer,
    -- | The list of tag keys and values associated with this launch.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the segment.
    arn :: Prelude.Text,
    -- | The date and time that this segment was created.
    createdTime :: Data.POSIX,
    -- | The date and time that this segment was most recently updated.
    lastUpdatedTime :: Data.POSIX,
    -- | The name of the segment.
    name :: Prelude.Text,
    -- | The pattern that defines the attributes to use to evalute whether a user
    -- session will be in the segment. For more information about the pattern
    -- syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html Segment rule pattern syntax>.
    pattern' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Segment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'segment_description' - The customer-created description for this segment.
--
-- 'experimentCount', 'segment_experimentCount' - The number of experiments that this segment is used in. This count
-- includes all current experiments, not just those that are currently
-- running.
--
-- 'launchCount', 'segment_launchCount' - The number of launches that this segment is used in. This count includes
-- all current launches, not just those that are currently running.
--
-- 'tags', 'segment_tags' - The list of tag keys and values associated with this launch.
--
-- 'arn', 'segment_arn' - The ARN of the segment.
--
-- 'createdTime', 'segment_createdTime' - The date and time that this segment was created.
--
-- 'lastUpdatedTime', 'segment_lastUpdatedTime' - The date and time that this segment was most recently updated.
--
-- 'name', 'segment_name' - The name of the segment.
--
-- 'pattern'', 'segment_pattern' - The pattern that defines the attributes to use to evalute whether a user
-- session will be in the segment. For more information about the pattern
-- syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html Segment rule pattern syntax>.
newSegment ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'pattern''
  Prelude.Text ->
  Segment
newSegment
  pArn_
  pCreatedTime_
  pLastUpdatedTime_
  pName_
  pPattern_ =
    Segment'
      { description = Prelude.Nothing,
        experimentCount = Prelude.Nothing,
        launchCount = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_,
        name = pName_,
        pattern' = pPattern_
      }

-- | The customer-created description for this segment.
segment_description :: Lens.Lens' Segment (Prelude.Maybe Prelude.Text)
segment_description = Lens.lens (\Segment' {description} -> description) (\s@Segment' {} a -> s {description = a} :: Segment)

-- | The number of experiments that this segment is used in. This count
-- includes all current experiments, not just those that are currently
-- running.
segment_experimentCount :: Lens.Lens' Segment (Prelude.Maybe Prelude.Integer)
segment_experimentCount = Lens.lens (\Segment' {experimentCount} -> experimentCount) (\s@Segment' {} a -> s {experimentCount = a} :: Segment)

-- | The number of launches that this segment is used in. This count includes
-- all current launches, not just those that are currently running.
segment_launchCount :: Lens.Lens' Segment (Prelude.Maybe Prelude.Integer)
segment_launchCount = Lens.lens (\Segment' {launchCount} -> launchCount) (\s@Segment' {} a -> s {launchCount = a} :: Segment)

-- | The list of tag keys and values associated with this launch.
segment_tags :: Lens.Lens' Segment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
segment_tags = Lens.lens (\Segment' {tags} -> tags) (\s@Segment' {} a -> s {tags = a} :: Segment) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the segment.
segment_arn :: Lens.Lens' Segment Prelude.Text
segment_arn = Lens.lens (\Segment' {arn} -> arn) (\s@Segment' {} a -> s {arn = a} :: Segment)

-- | The date and time that this segment was created.
segment_createdTime :: Lens.Lens' Segment Prelude.UTCTime
segment_createdTime = Lens.lens (\Segment' {createdTime} -> createdTime) (\s@Segment' {} a -> s {createdTime = a} :: Segment) Prelude.. Data._Time

-- | The date and time that this segment was most recently updated.
segment_lastUpdatedTime :: Lens.Lens' Segment Prelude.UTCTime
segment_lastUpdatedTime = Lens.lens (\Segment' {lastUpdatedTime} -> lastUpdatedTime) (\s@Segment' {} a -> s {lastUpdatedTime = a} :: Segment) Prelude.. Data._Time

-- | The name of the segment.
segment_name :: Lens.Lens' Segment Prelude.Text
segment_name = Lens.lens (\Segment' {name} -> name) (\s@Segment' {} a -> s {name = a} :: Segment)

-- | The pattern that defines the attributes to use to evalute whether a user
-- session will be in the segment. For more information about the pattern
-- syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html Segment rule pattern syntax>.
segment_pattern :: Lens.Lens' Segment Prelude.Text
segment_pattern = Lens.lens (\Segment' {pattern'} -> pattern') (\s@Segment' {} a -> s {pattern' = a} :: Segment)

instance Data.FromJSON Segment where
  parseJSON =
    Data.withObject
      "Segment"
      ( \x ->
          Segment'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "experimentCount")
            Prelude.<*> (x Data..:? "launchCount")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdTime")
            Prelude.<*> (x Data..: "lastUpdatedTime")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "pattern")
      )

instance Prelude.Hashable Segment where
  hashWithSalt _salt Segment' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` experimentCount
      `Prelude.hashWithSalt` launchCount
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData Segment where
  rnf Segment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf experimentCount
      `Prelude.seq` Prelude.rnf launchCount
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pattern'
