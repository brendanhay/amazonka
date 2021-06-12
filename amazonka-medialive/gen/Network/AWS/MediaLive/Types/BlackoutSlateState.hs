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
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlateState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BlackoutSlateState
  ( BlackoutSlateState
      ( ..,
        BlackoutSlateState_DISABLED,
        BlackoutSlateState_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Blackout Slate State
newtype BlackoutSlateState = BlackoutSlateState'
  { fromBlackoutSlateState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern BlackoutSlateState_DISABLED :: BlackoutSlateState
pattern BlackoutSlateState_DISABLED = BlackoutSlateState' "DISABLED"

pattern BlackoutSlateState_ENABLED :: BlackoutSlateState
pattern BlackoutSlateState_ENABLED = BlackoutSlateState' "ENABLED"

{-# COMPLETE
  BlackoutSlateState_DISABLED,
  BlackoutSlateState_ENABLED,
  BlackoutSlateState'
  #-}
