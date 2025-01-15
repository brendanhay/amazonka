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
-- Module      : Amazonka.Synthetics.Types.BaseScreenshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.BaseScreenshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure representing a screenshot that is used as a baseline during
-- visual monitoring comparisons made by the canary.
--
-- /See:/ 'newBaseScreenshot' smart constructor.
data BaseScreenshot = BaseScreenshot'
  { -- | Coordinates that define the part of a screen to ignore during screenshot
    -- comparisons. To obtain the coordinates to use here, use the CloudWatch
    -- console to draw the boundaries on the screen. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/synthetics_canaries_deletion.html Editing or deleting a canary>
    ignoreCoordinates :: Prelude.Maybe [Prelude.Text],
    -- | The name of the screenshot. This is generated the first time the canary
    -- is run after the @UpdateCanary@ operation that specified for this canary
    -- to perform visual monitoring.
    screenshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaseScreenshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignoreCoordinates', 'baseScreenshot_ignoreCoordinates' - Coordinates that define the part of a screen to ignore during screenshot
-- comparisons. To obtain the coordinates to use here, use the CloudWatch
-- console to draw the boundaries on the screen. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/synthetics_canaries_deletion.html Editing or deleting a canary>
--
-- 'screenshotName', 'baseScreenshot_screenshotName' - The name of the screenshot. This is generated the first time the canary
-- is run after the @UpdateCanary@ operation that specified for this canary
-- to perform visual monitoring.
newBaseScreenshot ::
  -- | 'screenshotName'
  Prelude.Text ->
  BaseScreenshot
newBaseScreenshot pScreenshotName_ =
  BaseScreenshot'
    { ignoreCoordinates =
        Prelude.Nothing,
      screenshotName = pScreenshotName_
    }

-- | Coordinates that define the part of a screen to ignore during screenshot
-- comparisons. To obtain the coordinates to use here, use the CloudWatch
-- console to draw the boundaries on the screen. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/synthetics_canaries_deletion.html Editing or deleting a canary>
baseScreenshot_ignoreCoordinates :: Lens.Lens' BaseScreenshot (Prelude.Maybe [Prelude.Text])
baseScreenshot_ignoreCoordinates = Lens.lens (\BaseScreenshot' {ignoreCoordinates} -> ignoreCoordinates) (\s@BaseScreenshot' {} a -> s {ignoreCoordinates = a} :: BaseScreenshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the screenshot. This is generated the first time the canary
-- is run after the @UpdateCanary@ operation that specified for this canary
-- to perform visual monitoring.
baseScreenshot_screenshotName :: Lens.Lens' BaseScreenshot Prelude.Text
baseScreenshot_screenshotName = Lens.lens (\BaseScreenshot' {screenshotName} -> screenshotName) (\s@BaseScreenshot' {} a -> s {screenshotName = a} :: BaseScreenshot)

instance Data.FromJSON BaseScreenshot where
  parseJSON =
    Data.withObject
      "BaseScreenshot"
      ( \x ->
          BaseScreenshot'
            Prelude.<$> ( x
                            Data..:? "IgnoreCoordinates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "ScreenshotName")
      )

instance Prelude.Hashable BaseScreenshot where
  hashWithSalt _salt BaseScreenshot' {..} =
    _salt
      `Prelude.hashWithSalt` ignoreCoordinates
      `Prelude.hashWithSalt` screenshotName

instance Prelude.NFData BaseScreenshot where
  rnf BaseScreenshot' {..} =
    Prelude.rnf ignoreCoordinates `Prelude.seq`
      Prelude.rnf screenshotName

instance Data.ToJSON BaseScreenshot where
  toJSON BaseScreenshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IgnoreCoordinates" Data..=)
              Prelude.<$> ignoreCoordinates,
            Prelude.Just
              ("ScreenshotName" Data..= screenshotName)
          ]
      )
