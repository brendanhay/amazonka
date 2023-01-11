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
-- Module      : Amazonka.AlexaBusiness.Types.Feature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Feature
  ( Feature
      ( ..,
        Feature_ALL,
        Feature_BLUETOOTH,
        Feature_LISTS,
        Feature_NETWORK_PROFILE,
        Feature_NOTIFICATIONS,
        Feature_SETTINGS,
        Feature_SKILLS,
        Feature_VOLUME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Feature = Feature' {fromFeature :: Data.Text}
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

pattern Feature_ALL :: Feature
pattern Feature_ALL = Feature' "ALL"

pattern Feature_BLUETOOTH :: Feature
pattern Feature_BLUETOOTH = Feature' "BLUETOOTH"

pattern Feature_LISTS :: Feature
pattern Feature_LISTS = Feature' "LISTS"

pattern Feature_NETWORK_PROFILE :: Feature
pattern Feature_NETWORK_PROFILE = Feature' "NETWORK_PROFILE"

pattern Feature_NOTIFICATIONS :: Feature
pattern Feature_NOTIFICATIONS = Feature' "NOTIFICATIONS"

pattern Feature_SETTINGS :: Feature
pattern Feature_SETTINGS = Feature' "SETTINGS"

pattern Feature_SKILLS :: Feature
pattern Feature_SKILLS = Feature' "SKILLS"

pattern Feature_VOLUME :: Feature
pattern Feature_VOLUME = Feature' "VOLUME"

{-# COMPLETE
  Feature_ALL,
  Feature_BLUETOOTH,
  Feature_LISTS,
  Feature_NETWORK_PROFILE,
  Feature_NOTIFICATIONS,
  Feature_SETTINGS,
  Feature_SKILLS,
  Feature_VOLUME,
  Feature'
  #-}
