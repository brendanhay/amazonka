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
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
  ( GlobalConfigurationInputEndAction
      ( ..,
        GlobalConfigurationInputEndAction_NONE,
        GlobalConfigurationInputEndAction_SWITCH_AND_LOOP_INPUTS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Global Configuration Input End Action
newtype GlobalConfigurationInputEndAction = GlobalConfigurationInputEndAction'
  { fromGlobalConfigurationInputEndAction ::
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

pattern GlobalConfigurationInputEndAction_NONE :: GlobalConfigurationInputEndAction
pattern GlobalConfigurationInputEndAction_NONE = GlobalConfigurationInputEndAction' "NONE"

pattern GlobalConfigurationInputEndAction_SWITCH_AND_LOOP_INPUTS :: GlobalConfigurationInputEndAction
pattern GlobalConfigurationInputEndAction_SWITCH_AND_LOOP_INPUTS = GlobalConfigurationInputEndAction' "SWITCH_AND_LOOP_INPUTS"

{-# COMPLETE
  GlobalConfigurationInputEndAction_NONE,
  GlobalConfigurationInputEndAction_SWITCH_AND_LOOP_INPUTS,
  GlobalConfigurationInputEndAction'
  #-}
