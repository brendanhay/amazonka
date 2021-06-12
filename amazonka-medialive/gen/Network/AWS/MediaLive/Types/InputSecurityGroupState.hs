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
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroupState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroupState
  ( InputSecurityGroupState
      ( ..,
        InputSecurityGroupState_DELETED,
        InputSecurityGroupState_IDLE,
        InputSecurityGroupState_IN_USE,
        InputSecurityGroupState_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Placeholder documentation for InputSecurityGroupState
newtype InputSecurityGroupState = InputSecurityGroupState'
  { fromInputSecurityGroupState ::
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

pattern InputSecurityGroupState_DELETED :: InputSecurityGroupState
pattern InputSecurityGroupState_DELETED = InputSecurityGroupState' "DELETED"

pattern InputSecurityGroupState_IDLE :: InputSecurityGroupState
pattern InputSecurityGroupState_IDLE = InputSecurityGroupState' "IDLE"

pattern InputSecurityGroupState_IN_USE :: InputSecurityGroupState
pattern InputSecurityGroupState_IN_USE = InputSecurityGroupState' "IN_USE"

pattern InputSecurityGroupState_UPDATING :: InputSecurityGroupState
pattern InputSecurityGroupState_UPDATING = InputSecurityGroupState' "UPDATING"

{-# COMPLETE
  InputSecurityGroupState_DELETED,
  InputSecurityGroupState_IDLE,
  InputSecurityGroupState_IN_USE,
  InputSecurityGroupState_UPDATING,
  InputSecurityGroupState'
  #-}
