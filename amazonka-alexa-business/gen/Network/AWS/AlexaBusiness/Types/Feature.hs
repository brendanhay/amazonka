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
-- Module      : Network.AWS.AlexaBusiness.Types.Feature
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Feature
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

import qualified Network.AWS.Prelude as Prelude

newtype Feature = Feature'
  { fromFeature ::
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
