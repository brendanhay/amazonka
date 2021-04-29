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
-- Module      : Network.AWS.S3.Types.DefaultRetention
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DefaultRetention where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | The container element for specifying the default Object Lock retention
-- settings for new objects placed in the specified bucket.
--
-- /See:/ 'newDefaultRetention' smart constructor.
data DefaultRetention = DefaultRetention'
  { -- | The number of days that you want to specify for the default retention
    -- period.
    days :: Prelude.Maybe Prelude.Int,
    -- | The number of years that you want to specify for the default retention
    -- period.
    years :: Prelude.Maybe Prelude.Int,
    -- | The default Object Lock retention mode you want to apply to new objects
    -- placed in the specified bucket.
    mode :: Prelude.Maybe ObjectLockRetentionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { days = Prelude.Nothing,
      years = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | The number of days that you want to specify for the default retention
-- period.
defaultRetention_days :: Lens.Lens' DefaultRetention (Prelude.Maybe Prelude.Int)
defaultRetention_days = Lens.lens (\DefaultRetention' {days} -> days) (\s@DefaultRetention' {} a -> s {days = a} :: DefaultRetention)

-- | The number of years that you want to specify for the default retention
-- period.
defaultRetention_years :: Lens.Lens' DefaultRetention (Prelude.Maybe Prelude.Int)
defaultRetention_years = Lens.lens (\DefaultRetention' {years} -> years) (\s@DefaultRetention' {} a -> s {years = a} :: DefaultRetention)

-- | The default Object Lock retention mode you want to apply to new objects
-- placed in the specified bucket.
defaultRetention_mode :: Lens.Lens' DefaultRetention (Prelude.Maybe ObjectLockRetentionMode)
defaultRetention_mode = Lens.lens (\DefaultRetention' {mode} -> mode) (\s@DefaultRetention' {} a -> s {mode = a} :: DefaultRetention)

instance Prelude.FromXML DefaultRetention where
  parseXML x =
    DefaultRetention'
      Prelude.<$> (x Prelude..@? "Days")
      Prelude.<*> (x Prelude..@? "Years")
      Prelude.<*> (x Prelude..@? "Mode")

instance Prelude.Hashable DefaultRetention

instance Prelude.NFData DefaultRetention

instance Prelude.ToXML DefaultRetention where
  toXML DefaultRetention' {..} =
    Prelude.mconcat
      [ "Days" Prelude.@= days,
        "Years" Prelude.@= years,
        "Mode" Prelude.@= mode
      ]
