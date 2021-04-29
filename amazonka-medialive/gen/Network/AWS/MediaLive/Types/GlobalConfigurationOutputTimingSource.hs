{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
  ( GlobalConfigurationOutputTimingSource
      ( ..,
        GlobalConfigurationOutputTimingSource_INPUT_CLOCK,
        GlobalConfigurationOutputTimingSource_SYSTEM_CLOCK
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Global Configuration Output Timing Source
newtype GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource'
  { fromGlobalConfigurationOutputTimingSource ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern GlobalConfigurationOutputTimingSource_INPUT_CLOCK :: GlobalConfigurationOutputTimingSource
pattern GlobalConfigurationOutputTimingSource_INPUT_CLOCK = GlobalConfigurationOutputTimingSource' "INPUT_CLOCK"

pattern GlobalConfigurationOutputTimingSource_SYSTEM_CLOCK :: GlobalConfigurationOutputTimingSource
pattern GlobalConfigurationOutputTimingSource_SYSTEM_CLOCK = GlobalConfigurationOutputTimingSource' "SYSTEM_CLOCK"

{-# COMPLETE
  GlobalConfigurationOutputTimingSource_INPUT_CLOCK,
  GlobalConfigurationOutputTimingSource_SYSTEM_CLOCK,
  GlobalConfigurationOutputTimingSource'
  #-}
