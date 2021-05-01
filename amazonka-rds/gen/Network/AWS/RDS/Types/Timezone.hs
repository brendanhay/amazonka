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
-- Module      : Network.AWS.RDS.Types.Timezone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Timezone where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A time zone associated with a @DBInstance@ or a @DBSnapshot@. This data
-- type is an element in the response to the @DescribeDBInstances@, the
-- @DescribeDBSnapshots@, and the @DescribeDBEngineVersions@ actions.
--
-- /See:/ 'newTimezone' smart constructor.
data Timezone = Timezone'
  { -- | The name of the time zone.
    timezoneName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Timezone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timezoneName', 'timezone_timezoneName' - The name of the time zone.
newTimezone ::
  Timezone
newTimezone =
  Timezone' {timezoneName = Prelude.Nothing}

-- | The name of the time zone.
timezone_timezoneName :: Lens.Lens' Timezone (Prelude.Maybe Prelude.Text)
timezone_timezoneName = Lens.lens (\Timezone' {timezoneName} -> timezoneName) (\s@Timezone' {} a -> s {timezoneName = a} :: Timezone)

instance Prelude.FromXML Timezone where
  parseXML x =
    Timezone'
      Prelude.<$> (x Prelude..@? "TimezoneName")

instance Prelude.Hashable Timezone

instance Prelude.NFData Timezone
