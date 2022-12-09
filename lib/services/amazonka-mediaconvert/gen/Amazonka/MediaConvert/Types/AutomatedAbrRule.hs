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
-- Module      : Amazonka.MediaConvert.Types.AutomatedAbrRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AutomatedAbrRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AllowedRenditionSize
import Amazonka.MediaConvert.Types.ForceIncludeRenditionSize
import Amazonka.MediaConvert.Types.MinBottomRenditionSize
import Amazonka.MediaConvert.Types.MinTopRenditionSize
import Amazonka.MediaConvert.Types.RuleType
import qualified Amazonka.Prelude as Prelude

-- | Specify one or more Automated ABR rule types. Note: Force include and
-- Allowed renditions are mutually exclusive.
--
-- /See:/ 'newAutomatedAbrRule' smart constructor.
data AutomatedAbrRule = AutomatedAbrRule'
  { -- | When customer adds the allowed renditions rule for auto ABR ladder, they
    -- are required to add at leat one rendition to allowedRenditions list
    allowedRenditions :: Prelude.Maybe [AllowedRenditionSize],
    -- | When customer adds the force include renditions rule for auto ABR
    -- ladder, they are required to add at leat one rendition to
    -- forceIncludeRenditions list
    forceIncludeRenditions :: Prelude.Maybe [ForceIncludeRenditionSize],
    -- | Use Min bottom rendition size to specify a minimum size for the lowest
    -- resolution in your ABR stack. * The lowest resolution in your ABR stack
    -- will be equal to or greater than the value that you enter. For example:
    -- If you specify 640x360 the lowest resolution in your ABR stack will be
    -- equal to or greater than to 640x360. * If you specify a Min top
    -- rendition size rule, the value that you specify for Min bottom rendition
    -- size must be less than, or equal to, Min top rendition size.
    minBottomRenditionSize :: Prelude.Maybe MinBottomRenditionSize,
    -- | Use Min top rendition size to specify a minimum size for the highest
    -- resolution in your ABR stack. * The highest resolution in your ABR stack
    -- will be equal to or greater than the value that you enter. For example:
    -- If you specify 1280x720 the highest resolution in your ABR stack will be
    -- equal to or greater than 1280x720. * If you specify a value for Max
    -- resolution, the value that you specify for Min top rendition size must
    -- be less than, or equal to, Max resolution.
    minTopRenditionSize :: Prelude.Maybe MinTopRenditionSize,
    -- | Use Min top rendition size to specify a minimum size for the highest
    -- resolution in your ABR stack. * The highest resolution in your ABR stack
    -- will be equal to or greater than the value that you enter. For example:
    -- If you specify 1280x720 the highest resolution in your ABR stack will be
    -- equal to or greater than 1280x720. * If you specify a value for Max
    -- resolution, the value that you specify for Min top rendition size must
    -- be less than, or equal to, Max resolution. Use Min bottom rendition size
    -- to specify a minimum size for the lowest resolution in your ABR stack. *
    -- The lowest resolution in your ABR stack will be equal to or greater than
    -- the value that you enter. For example: If you specify 640x360 the lowest
    -- resolution in your ABR stack will be equal to or greater than to
    -- 640x360. * If you specify a Min top rendition size rule, the value that
    -- you specify for Min bottom rendition size must be less than, or equal
    -- to, Min top rendition size. Use Force include renditions to specify one
    -- or more resolutions to include your ABR stack. * (Recommended) To
    -- optimize automated ABR, specify as few resolutions as possible. *
    -- (Required) The number of resolutions that you specify must be equal to,
    -- or less than, the Max renditions setting. * If you specify a Min top
    -- rendition size rule, specify at least one resolution that is equal to,
    -- or greater than, Min top rendition size. * If you specify a Min bottom
    -- rendition size rule, only specify resolutions that are equal to, or
    -- greater than, Min bottom rendition size. * If you specify a Force
    -- include renditions rule, do not specify a separate rule for Allowed
    -- renditions. * Note: The ABR stack may include other resolutions that you
    -- do not specify here, depending on the Max renditions setting. Use
    -- Allowed renditions to specify a list of possible resolutions in your ABR
    -- stack. * (Required) The number of resolutions that you specify must be
    -- equal to, or greater than, the Max renditions setting. * MediaConvert
    -- will create an ABR stack exclusively from the list of resolutions that
    -- you specify. * Some resolutions in the Allowed renditions list may not
    -- be included, however you can force a resolution to be included by
    -- setting Required to ENABLED. * You must specify at least one resolution
    -- that is greater than or equal to any resolutions that you specify in Min
    -- top rendition size or Min bottom rendition size. * If you specify
    -- Allowed renditions, you must not specify a separate rule for Force
    -- include renditions.
    type' :: Prelude.Maybe RuleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomatedAbrRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedRenditions', 'automatedAbrRule_allowedRenditions' - When customer adds the allowed renditions rule for auto ABR ladder, they
-- are required to add at leat one rendition to allowedRenditions list
--
-- 'forceIncludeRenditions', 'automatedAbrRule_forceIncludeRenditions' - When customer adds the force include renditions rule for auto ABR
-- ladder, they are required to add at leat one rendition to
-- forceIncludeRenditions list
--
-- 'minBottomRenditionSize', 'automatedAbrRule_minBottomRenditionSize' - Use Min bottom rendition size to specify a minimum size for the lowest
-- resolution in your ABR stack. * The lowest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 640x360 the lowest resolution in your ABR stack will be
-- equal to or greater than to 640x360. * If you specify a Min top
-- rendition size rule, the value that you specify for Min bottom rendition
-- size must be less than, or equal to, Min top rendition size.
--
-- 'minTopRenditionSize', 'automatedAbrRule_minTopRenditionSize' - Use Min top rendition size to specify a minimum size for the highest
-- resolution in your ABR stack. * The highest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 1280x720 the highest resolution in your ABR stack will be
-- equal to or greater than 1280x720. * If you specify a value for Max
-- resolution, the value that you specify for Min top rendition size must
-- be less than, or equal to, Max resolution.
--
-- 'type'', 'automatedAbrRule_type' - Use Min top rendition size to specify a minimum size for the highest
-- resolution in your ABR stack. * The highest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 1280x720 the highest resolution in your ABR stack will be
-- equal to or greater than 1280x720. * If you specify a value for Max
-- resolution, the value that you specify for Min top rendition size must
-- be less than, or equal to, Max resolution. Use Min bottom rendition size
-- to specify a minimum size for the lowest resolution in your ABR stack. *
-- The lowest resolution in your ABR stack will be equal to or greater than
-- the value that you enter. For example: If you specify 640x360 the lowest
-- resolution in your ABR stack will be equal to or greater than to
-- 640x360. * If you specify a Min top rendition size rule, the value that
-- you specify for Min bottom rendition size must be less than, or equal
-- to, Min top rendition size. Use Force include renditions to specify one
-- or more resolutions to include your ABR stack. * (Recommended) To
-- optimize automated ABR, specify as few resolutions as possible. *
-- (Required) The number of resolutions that you specify must be equal to,
-- or less than, the Max renditions setting. * If you specify a Min top
-- rendition size rule, specify at least one resolution that is equal to,
-- or greater than, Min top rendition size. * If you specify a Min bottom
-- rendition size rule, only specify resolutions that are equal to, or
-- greater than, Min bottom rendition size. * If you specify a Force
-- include renditions rule, do not specify a separate rule for Allowed
-- renditions. * Note: The ABR stack may include other resolutions that you
-- do not specify here, depending on the Max renditions setting. Use
-- Allowed renditions to specify a list of possible resolutions in your ABR
-- stack. * (Required) The number of resolutions that you specify must be
-- equal to, or greater than, the Max renditions setting. * MediaConvert
-- will create an ABR stack exclusively from the list of resolutions that
-- you specify. * Some resolutions in the Allowed renditions list may not
-- be included, however you can force a resolution to be included by
-- setting Required to ENABLED. * You must specify at least one resolution
-- that is greater than or equal to any resolutions that you specify in Min
-- top rendition size or Min bottom rendition size. * If you specify
-- Allowed renditions, you must not specify a separate rule for Force
-- include renditions.
newAutomatedAbrRule ::
  AutomatedAbrRule
newAutomatedAbrRule =
  AutomatedAbrRule'
    { allowedRenditions =
        Prelude.Nothing,
      forceIncludeRenditions = Prelude.Nothing,
      minBottomRenditionSize = Prelude.Nothing,
      minTopRenditionSize = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | When customer adds the allowed renditions rule for auto ABR ladder, they
-- are required to add at leat one rendition to allowedRenditions list
automatedAbrRule_allowedRenditions :: Lens.Lens' AutomatedAbrRule (Prelude.Maybe [AllowedRenditionSize])
automatedAbrRule_allowedRenditions = Lens.lens (\AutomatedAbrRule' {allowedRenditions} -> allowedRenditions) (\s@AutomatedAbrRule' {} a -> s {allowedRenditions = a} :: AutomatedAbrRule) Prelude.. Lens.mapping Lens.coerced

-- | When customer adds the force include renditions rule for auto ABR
-- ladder, they are required to add at leat one rendition to
-- forceIncludeRenditions list
automatedAbrRule_forceIncludeRenditions :: Lens.Lens' AutomatedAbrRule (Prelude.Maybe [ForceIncludeRenditionSize])
automatedAbrRule_forceIncludeRenditions = Lens.lens (\AutomatedAbrRule' {forceIncludeRenditions} -> forceIncludeRenditions) (\s@AutomatedAbrRule' {} a -> s {forceIncludeRenditions = a} :: AutomatedAbrRule) Prelude.. Lens.mapping Lens.coerced

-- | Use Min bottom rendition size to specify a minimum size for the lowest
-- resolution in your ABR stack. * The lowest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 640x360 the lowest resolution in your ABR stack will be
-- equal to or greater than to 640x360. * If you specify a Min top
-- rendition size rule, the value that you specify for Min bottom rendition
-- size must be less than, or equal to, Min top rendition size.
automatedAbrRule_minBottomRenditionSize :: Lens.Lens' AutomatedAbrRule (Prelude.Maybe MinBottomRenditionSize)
automatedAbrRule_minBottomRenditionSize = Lens.lens (\AutomatedAbrRule' {minBottomRenditionSize} -> minBottomRenditionSize) (\s@AutomatedAbrRule' {} a -> s {minBottomRenditionSize = a} :: AutomatedAbrRule)

-- | Use Min top rendition size to specify a minimum size for the highest
-- resolution in your ABR stack. * The highest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 1280x720 the highest resolution in your ABR stack will be
-- equal to or greater than 1280x720. * If you specify a value for Max
-- resolution, the value that you specify for Min top rendition size must
-- be less than, or equal to, Max resolution.
automatedAbrRule_minTopRenditionSize :: Lens.Lens' AutomatedAbrRule (Prelude.Maybe MinTopRenditionSize)
automatedAbrRule_minTopRenditionSize = Lens.lens (\AutomatedAbrRule' {minTopRenditionSize} -> minTopRenditionSize) (\s@AutomatedAbrRule' {} a -> s {minTopRenditionSize = a} :: AutomatedAbrRule)

-- | Use Min top rendition size to specify a minimum size for the highest
-- resolution in your ABR stack. * The highest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 1280x720 the highest resolution in your ABR stack will be
-- equal to or greater than 1280x720. * If you specify a value for Max
-- resolution, the value that you specify for Min top rendition size must
-- be less than, or equal to, Max resolution. Use Min bottom rendition size
-- to specify a minimum size for the lowest resolution in your ABR stack. *
-- The lowest resolution in your ABR stack will be equal to or greater than
-- the value that you enter. For example: If you specify 640x360 the lowest
-- resolution in your ABR stack will be equal to or greater than to
-- 640x360. * If you specify a Min top rendition size rule, the value that
-- you specify for Min bottom rendition size must be less than, or equal
-- to, Min top rendition size. Use Force include renditions to specify one
-- or more resolutions to include your ABR stack. * (Recommended) To
-- optimize automated ABR, specify as few resolutions as possible. *
-- (Required) The number of resolutions that you specify must be equal to,
-- or less than, the Max renditions setting. * If you specify a Min top
-- rendition size rule, specify at least one resolution that is equal to,
-- or greater than, Min top rendition size. * If you specify a Min bottom
-- rendition size rule, only specify resolutions that are equal to, or
-- greater than, Min bottom rendition size. * If you specify a Force
-- include renditions rule, do not specify a separate rule for Allowed
-- renditions. * Note: The ABR stack may include other resolutions that you
-- do not specify here, depending on the Max renditions setting. Use
-- Allowed renditions to specify a list of possible resolutions in your ABR
-- stack. * (Required) The number of resolutions that you specify must be
-- equal to, or greater than, the Max renditions setting. * MediaConvert
-- will create an ABR stack exclusively from the list of resolutions that
-- you specify. * Some resolutions in the Allowed renditions list may not
-- be included, however you can force a resolution to be included by
-- setting Required to ENABLED. * You must specify at least one resolution
-- that is greater than or equal to any resolutions that you specify in Min
-- top rendition size or Min bottom rendition size. * If you specify
-- Allowed renditions, you must not specify a separate rule for Force
-- include renditions.
automatedAbrRule_type :: Lens.Lens' AutomatedAbrRule (Prelude.Maybe RuleType)
automatedAbrRule_type = Lens.lens (\AutomatedAbrRule' {type'} -> type') (\s@AutomatedAbrRule' {} a -> s {type' = a} :: AutomatedAbrRule)

instance Data.FromJSON AutomatedAbrRule where
  parseJSON =
    Data.withObject
      "AutomatedAbrRule"
      ( \x ->
          AutomatedAbrRule'
            Prelude.<$> ( x Data..:? "allowedRenditions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "forceIncludeRenditions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "minBottomRenditionSize")
            Prelude.<*> (x Data..:? "minTopRenditionSize")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable AutomatedAbrRule where
  hashWithSalt _salt AutomatedAbrRule' {..} =
    _salt `Prelude.hashWithSalt` allowedRenditions
      `Prelude.hashWithSalt` forceIncludeRenditions
      `Prelude.hashWithSalt` minBottomRenditionSize
      `Prelude.hashWithSalt` minTopRenditionSize
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AutomatedAbrRule where
  rnf AutomatedAbrRule' {..} =
    Prelude.rnf allowedRenditions
      `Prelude.seq` Prelude.rnf forceIncludeRenditions
      `Prelude.seq` Prelude.rnf minBottomRenditionSize
      `Prelude.seq` Prelude.rnf minTopRenditionSize
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AutomatedAbrRule where
  toJSON AutomatedAbrRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowedRenditions" Data..=)
              Prelude.<$> allowedRenditions,
            ("forceIncludeRenditions" Data..=)
              Prelude.<$> forceIncludeRenditions,
            ("minBottomRenditionSize" Data..=)
              Prelude.<$> minBottomRenditionSize,
            ("minTopRenditionSize" Data..=)
              Prelude.<$> minTopRenditionSize,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
