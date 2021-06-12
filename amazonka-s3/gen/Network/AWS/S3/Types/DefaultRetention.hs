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
-- Module      : Network.AWS.S3.Types.DefaultRetention
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DefaultRetention where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | The container element for specifying the default Object Lock retention
-- settings for new objects placed in the specified bucket.
--
-- /See:/ 'newDefaultRetention' smart constructor.
data DefaultRetention = DefaultRetention'
  { -- | The number of days that you want to specify for the default retention
    -- period.
    days :: Core.Maybe Core.Int,
    -- | The number of years that you want to specify for the default retention
    -- period.
    years :: Core.Maybe Core.Int,
    -- | The default Object Lock retention mode you want to apply to new objects
    -- placed in the specified bucket.
    mode :: Core.Maybe ObjectLockRetentionMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DefaultRetention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'defaultRetention_days' - The number of days that you want to specify for the default retention
-- period.
--
-- 'years', 'defaultRetention_years' - The number of years that you want to specify for the default retention
-- period.
--
-- 'mode', 'defaultRetention_mode' - The default Object Lock retention mode you want to apply to new objects
-- placed in the specified bucket.
newDefaultRetention ::
  DefaultRetention
newDefaultRetention =
  DefaultRetention'
    { days = Core.Nothing,
      years = Core.Nothing,
      mode = Core.Nothing
    }

-- | The number of days that you want to specify for the default retention
-- period.
defaultRetention_days :: Lens.Lens' DefaultRetention (Core.Maybe Core.Int)
defaultRetention_days = Lens.lens (\DefaultRetention' {days} -> days) (\s@DefaultRetention' {} a -> s {days = a} :: DefaultRetention)

-- | The number of years that you want to specify for the default retention
-- period.
defaultRetention_years :: Lens.Lens' DefaultRetention (Core.Maybe Core.Int)
defaultRetention_years = Lens.lens (\DefaultRetention' {years} -> years) (\s@DefaultRetention' {} a -> s {years = a} :: DefaultRetention)

-- | The default Object Lock retention mode you want to apply to new objects
-- placed in the specified bucket.
defaultRetention_mode :: Lens.Lens' DefaultRetention (Core.Maybe ObjectLockRetentionMode)
defaultRetention_mode = Lens.lens (\DefaultRetention' {mode} -> mode) (\s@DefaultRetention' {} a -> s {mode = a} :: DefaultRetention)

instance Core.FromXML DefaultRetention where
  parseXML x =
    DefaultRetention'
      Core.<$> (x Core..@? "Days")
      Core.<*> (x Core..@? "Years")
      Core.<*> (x Core..@? "Mode")

instance Core.Hashable DefaultRetention

instance Core.NFData DefaultRetention

instance Core.ToXML DefaultRetention where
  toXML DefaultRetention' {..} =
    Core.mconcat
      [ "Days" Core.@= days,
        "Years" Core.@= years,
        "Mode" Core.@= mode
      ]
