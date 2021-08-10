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
-- Module      : Network.AWS.DMS.Types.EventCategoryGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EventCategoryGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lists categories of events subscribed to, and generated by, the
-- applicable AWS DMS resource type. This data type appears in response to
-- the
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_EventCategoryGroup.html DescribeEventCategories>
-- action.
--
-- /See:/ 'newEventCategoryGroup' smart constructor.
data EventCategoryGroup = EventCategoryGroup'
  { -- | A list of event categories from a source type that you\'ve chosen.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-server | security-group
    -- | replication-task
    sourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventCategoryGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCategories', 'eventCategoryGroup_eventCategories' - A list of event categories from a source type that you\'ve chosen.
--
-- 'sourceType', 'eventCategoryGroup_sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group
-- | replication-task
newEventCategoryGroup ::
  EventCategoryGroup
newEventCategoryGroup =
  EventCategoryGroup'
    { eventCategories =
        Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | A list of event categories from a source type that you\'ve chosen.
eventCategoryGroup_eventCategories :: Lens.Lens' EventCategoryGroup (Prelude.Maybe [Prelude.Text])
eventCategoryGroup_eventCategories = Lens.lens (\EventCategoryGroup' {eventCategories} -> eventCategories) (\s@EventCategoryGroup' {} a -> s {eventCategories = a} :: EventCategoryGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group
-- | replication-task
eventCategoryGroup_sourceType :: Lens.Lens' EventCategoryGroup (Prelude.Maybe Prelude.Text)
eventCategoryGroup_sourceType = Lens.lens (\EventCategoryGroup' {sourceType} -> sourceType) (\s@EventCategoryGroup' {} a -> s {sourceType = a} :: EventCategoryGroup)

instance Core.FromJSON EventCategoryGroup where
  parseJSON =
    Core.withObject
      "EventCategoryGroup"
      ( \x ->
          EventCategoryGroup'
            Prelude.<$> ( x Core..:? "EventCategories"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SourceType")
      )

instance Prelude.Hashable EventCategoryGroup

instance Prelude.NFData EventCategoryGroup
