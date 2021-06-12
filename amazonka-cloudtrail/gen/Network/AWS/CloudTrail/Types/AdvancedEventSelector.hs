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
-- Module      : Network.AWS.CloudTrail.Types.AdvancedEventSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedEventSelector where

import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Advanced event selectors let you create fine-grained selectors for the
-- following AWS CloudTrail event record ﬁelds. They help you control costs
-- by logging only those events that are important to you. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /AWS CloudTrail User Guide/.
--
-- -   @readOnly@
--
-- -   @eventSource@
--
-- -   @eventName@
--
-- -   @eventCategory@
--
-- -   @resources.type@
--
-- -   @resources.ARN@
--
-- You cannot apply both event selectors and advanced event selectors to a
-- trail.
--
-- /See:/ 'newAdvancedEventSelector' smart constructor.
data AdvancedEventSelector = AdvancedEventSelector'
  { -- | An optional, descriptive name for an advanced event selector, such as
    -- \"Log data events for only two S3 buckets\".
    name :: Core.Maybe Core.Text,
    -- | Contains all selector statements in an advanced event selector.
    fieldSelectors :: Core.NonEmpty AdvancedFieldSelector
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdvancedEventSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'advancedEventSelector_name' - An optional, descriptive name for an advanced event selector, such as
-- \"Log data events for only two S3 buckets\".
--
-- 'fieldSelectors', 'advancedEventSelector_fieldSelectors' - Contains all selector statements in an advanced event selector.
newAdvancedEventSelector ::
  -- | 'fieldSelectors'
  Core.NonEmpty AdvancedFieldSelector ->
  AdvancedEventSelector
newAdvancedEventSelector pFieldSelectors_ =
  AdvancedEventSelector'
    { name = Core.Nothing,
      fieldSelectors =
        Lens._Coerce Lens.# pFieldSelectors_
    }

-- | An optional, descriptive name for an advanced event selector, such as
-- \"Log data events for only two S3 buckets\".
advancedEventSelector_name :: Lens.Lens' AdvancedEventSelector (Core.Maybe Core.Text)
advancedEventSelector_name = Lens.lens (\AdvancedEventSelector' {name} -> name) (\s@AdvancedEventSelector' {} a -> s {name = a} :: AdvancedEventSelector)

-- | Contains all selector statements in an advanced event selector.
advancedEventSelector_fieldSelectors :: Lens.Lens' AdvancedEventSelector (Core.NonEmpty AdvancedFieldSelector)
advancedEventSelector_fieldSelectors = Lens.lens (\AdvancedEventSelector' {fieldSelectors} -> fieldSelectors) (\s@AdvancedEventSelector' {} a -> s {fieldSelectors = a} :: AdvancedEventSelector) Core.. Lens._Coerce

instance Core.FromJSON AdvancedEventSelector where
  parseJSON =
    Core.withObject
      "AdvancedEventSelector"
      ( \x ->
          AdvancedEventSelector'
            Core.<$> (x Core..:? "Name")
            Core.<*> (x Core..: "FieldSelectors")
      )

instance Core.Hashable AdvancedEventSelector

instance Core.NFData AdvancedEventSelector

instance Core.ToJSON AdvancedEventSelector where
  toJSON AdvancedEventSelector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            Core.Just ("FieldSelectors" Core..= fieldSelectors)
          ]
      )
