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
-- Module      : Amazonka.AWSHealth.Types.EntityFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EntityFilter where

import Amazonka.AWSHealth.Types.DateTimeRange
import Amazonka.AWSHealth.Types.EntityStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The values to use to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntities.html DescribeAffectedEntities>
-- operation.
--
-- /See:/ 'newEntityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { -- | A list of entity ARNs (unique identifiers).
    entityArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of IDs for affected entities.
    entityValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of the most recent dates and times that the entity was updated.
    lastUpdatedTimes :: Prelude.Maybe (Prelude.NonEmpty DateTimeRange),
    -- | A list of entity status codes (@IMPAIRED@, @UNIMPAIRED@, or @UNKNOWN@).
    statusCodes :: Prelude.Maybe (Prelude.NonEmpty EntityStatusCode),
    -- | A map of entity tags attached to the affected entity.
    --
    -- Currently, the @tags@ property isn\'t supported.
    tags :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'entityValues', 'entityFilter_entityValues' - A list of IDs for affected entities.
--
-- 'lastUpdatedTimes', 'entityFilter_lastUpdatedTimes' - A list of the most recent dates and times that the entity was updated.
--
-- 'statusCodes', 'entityFilter_statusCodes' - A list of entity status codes (@IMPAIRED@, @UNIMPAIRED@, or @UNKNOWN@).
--
-- 'tags', 'entityFilter_tags' - A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
--
-- 'eventArns', 'entityFilter_eventArns' - A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
newEntityFilter ::
  -- | 'eventArns'
  Prelude.NonEmpty Prelude.Text ->
  EntityFilter
newEntityFilter pEventArns_ =
  EntityFilter'
    { entityArns = Prelude.Nothing,
      entityValues = Prelude.Nothing,
      lastUpdatedTimes = Prelude.Nothing,
      statusCodes = Prelude.Nothing,
      tags = Prelude.Nothing,
      eventArns = Lens.coerced Lens.# pEventArns_
    }

-- | A list of entity ARNs (unique identifiers).
entityFilter_entityArns :: Lens.Lens' EntityFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
entityFilter_entityArns = Lens.lens (\EntityFilter' {entityArns} -> entityArns) (\s@EntityFilter' {} a -> s {entityArns = a} :: EntityFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of IDs for affected entities.
entityFilter_entityValues :: Lens.Lens' EntityFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
entityFilter_entityValues = Lens.lens (\EntityFilter' {entityValues} -> entityValues) (\s@EntityFilter' {} a -> s {entityValues = a} :: EntityFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of the most recent dates and times that the entity was updated.
entityFilter_lastUpdatedTimes :: Lens.Lens' EntityFilter (Prelude.Maybe (Prelude.NonEmpty DateTimeRange))
entityFilter_lastUpdatedTimes = Lens.lens (\EntityFilter' {lastUpdatedTimes} -> lastUpdatedTimes) (\s@EntityFilter' {} a -> s {lastUpdatedTimes = a} :: EntityFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of entity status codes (@IMPAIRED@, @UNIMPAIRED@, or @UNKNOWN@).
entityFilter_statusCodes :: Lens.Lens' EntityFilter (Prelude.Maybe (Prelude.NonEmpty EntityStatusCode))
entityFilter_statusCodes = Lens.lens (\EntityFilter' {statusCodes} -> statusCodes) (\s@EntityFilter' {} a -> s {statusCodes = a} :: EntityFilter) Prelude.. Lens.mapping Lens.coerced

-- | A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
entityFilter_tags :: Lens.Lens' EntityFilter (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
entityFilter_tags = Lens.lens (\EntityFilter' {tags} -> tags) (\s@EntityFilter' {} a -> s {tags = a} :: EntityFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
entityFilter_eventArns :: Lens.Lens' EntityFilter (Prelude.NonEmpty Prelude.Text)
entityFilter_eventArns = Lens.lens (\EntityFilter' {eventArns} -> eventArns) (\s@EntityFilter' {} a -> s {eventArns = a} :: EntityFilter) Prelude.. Lens.coerced

instance Prelude.Hashable EntityFilter where
  hashWithSalt _salt EntityFilter' {..} =
    _salt
      `Prelude.hashWithSalt` entityArns
      `Prelude.hashWithSalt` entityValues
      `Prelude.hashWithSalt` lastUpdatedTimes
      `Prelude.hashWithSalt` statusCodes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` eventArns

instance Prelude.NFData EntityFilter where
  rnf EntityFilter' {..} =
    Prelude.rnf entityArns
      `Prelude.seq` Prelude.rnf entityValues
      `Prelude.seq` Prelude.rnf lastUpdatedTimes
      `Prelude.seq` Prelude.rnf statusCodes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf eventArns

instance Data.ToJSON EntityFilter where
  toJSON EntityFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("entityArns" Data..=) Prelude.<$> entityArns,
            ("entityValues" Data..=) Prelude.<$> entityValues,
            ("lastUpdatedTimes" Data..=)
              Prelude.<$> lastUpdatedTimes,
            ("statusCodes" Data..=) Prelude.<$> statusCodes,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("eventArns" Data..= eventArns)
          ]
      )
