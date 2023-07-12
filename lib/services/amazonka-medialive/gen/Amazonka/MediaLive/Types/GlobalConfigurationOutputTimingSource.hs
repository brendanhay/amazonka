{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.GlobalConfigurationOutputTimingSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.GlobalConfigurationOutputTimingSource
  ( GlobalConfigurationOutputTimingSource
      ( ..,
        GlobalConfigurationOutputTimingSource_INPUT_CLOCK,
        GlobalConfigurationOutputTimingSource_SYSTEM_CLOCK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Global Configuration Output Timing Source
newtype GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource'
  { fromGlobalConfigurationOutputTimingSource ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
