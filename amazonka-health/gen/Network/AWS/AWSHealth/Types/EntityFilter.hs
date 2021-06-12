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
-- Module      : Network.AWS.AWSHealth.Types.EntityFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityFilter where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EntityStatusCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The values to use to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter>
-- operation.
--
-- /See:/ 'newEntityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { -- | A list of entity ARNs (unique identifiers).
    entityArns :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of entity status codes (@IMPAIRED@, @UNIMPAIRED@, or @UNKNOWN@).
    statusCodes :: Core.Maybe (Core.NonEmpty EntityStatusCode),
    -- | A list of the most recent dates and times that the entity was updated.
    lastUpdatedTimes :: Core.Maybe (Core.NonEmpty DateTimeRange),
    -- | A map of entity tags attached to the affected entity.
    --
    -- Currently, the @tags@ property isn\'t supported.
    tags :: Core.Maybe [Core.HashMap Core.Text Core.Text],
    -- | A list of IDs for affected entities.
    entityValues :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityArns', 'entityFilter_entityArns' - A list of entity ARNs (unique identifiers).
--
-- 'statusCodes', 'entityFilter_statusCodes' - A list of entity status codes (@IMPAIRED@, @UNIMPAIRED@, or @UNKNOWN@).
--
-- 'lastUpdatedTimes', 'entityFilter_lastUpdatedTimes' - A list of the most recent dates and times that the entity was updated.
--
-- 'tags', 'entityFilter_tags' - A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
--
-- 'entityValues', 'entityFilter_entityValues' - A list of IDs for affected entities.
--
-- 'eventArns', 'entityFilter_eventArns' - A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
newEntityFilter ::
  -- | 'eventArns'
  Core.NonEmpty Core.Text ->
  EntityFilter
newEntityFilter pEventArns_ =
  EntityFilter'
    { entityArns = Core.Nothing,
      statusCodes = Core.Nothing,
      lastUpdatedTimes = Core.Nothing,
      tags = Core.Nothing,
      entityValues = Core.Nothing,
      eventArns = Lens._Coerce Lens.# pEventArns_
    }

-- | A list of entity ARNs (unique identifiers).
entityFilter_entityArns :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty Core.Text))
entityFilter_entityArns = Lens.lens (\EntityFilter' {entityArns} -> entityArns) (\s@EntityFilter' {} a -> s {entityArns = a} :: EntityFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of entity status codes (@IMPAIRED@, @UNIMPAIRED@, or @UNKNOWN@).
entityFilter_statusCodes :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty EntityStatusCode))
entityFilter_statusCodes = Lens.lens (\EntityFilter' {statusCodes} -> statusCodes) (\s@EntityFilter' {} a -> s {statusCodes = a} :: EntityFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of the most recent dates and times that the entity was updated.
entityFilter_lastUpdatedTimes :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty DateTimeRange))
entityFilter_lastUpdatedTimes = Lens.lens (\EntityFilter' {lastUpdatedTimes} -> lastUpdatedTimes) (\s@EntityFilter' {} a -> s {lastUpdatedTimes = a} :: EntityFilter) Core.. Lens.mapping Lens._Coerce

-- | A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
entityFilter_tags :: Lens.Lens' EntityFilter (Core.Maybe [Core.HashMap Core.Text Core.Text])
entityFilter_tags = Lens.lens (\EntityFilter' {tags} -> tags) (\s@EntityFilter' {} a -> s {tags = a} :: EntityFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of IDs for affected entities.
entityFilter_entityValues :: Lens.Lens' EntityFilter (Core.Maybe (Core.NonEmpty Core.Text))
entityFilter_entityValues = Lens.lens (\EntityFilter' {entityValues} -> entityValues) (\s@EntityFilter' {} a -> s {entityValues = a} :: EntityFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
entityFilter_eventArns :: Lens.Lens' EntityFilter (Core.NonEmpty Core.Text)
entityFilter_eventArns = Lens.lens (\EntityFilter' {eventArns} -> eventArns) (\s@EntityFilter' {} a -> s {eventArns = a} :: EntityFilter) Core.. Lens._Coerce

instance Core.Hashable EntityFilter

instance Core.NFData EntityFilter

instance Core.ToJSON EntityFilter where
  toJSON EntityFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("entityArns" Core..=) Core.<$> entityArns,
            ("statusCodes" Core..=) Core.<$> statusCodes,
            ("lastUpdatedTimes" Core..=)
              Core.<$> lastUpdatedTimes,
            ("tags" Core..=) Core.<$> tags,
            ("entityValues" Core..=) Core.<$> entityValues,
            Core.Just ("eventArns" Core..= eventArns)
          ]
      )
