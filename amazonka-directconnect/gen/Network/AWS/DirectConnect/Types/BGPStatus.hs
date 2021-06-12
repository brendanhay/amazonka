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
-- Module      : Network.AWS.DirectConnect.Types.BGPStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPStatus
  ( BGPStatus
      ( ..,
        BGPStatus_Down,
        BGPStatus_Unknown,
        BGPStatus_Up
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype BGPStatus = BGPStatus'
  { fromBGPStatus ::
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

pattern BGPStatus_Down :: BGPStatus
pattern BGPStatus_Down = BGPStatus' "down"

pattern BGPStatus_Unknown :: BGPStatus
pattern BGPStatus_Unknown = BGPStatus' "unknown"

pattern BGPStatus_Up :: BGPStatus
pattern BGPStatus_Up = BGPStatus' "up"

{-# COMPLETE
  BGPStatus_Down,
  BGPStatus_Unknown,
  BGPStatus_Up,
  BGPStatus'
  #-}
