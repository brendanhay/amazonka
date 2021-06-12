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
-- Module      : Network.AWS.SSM.Types.PingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PingStatus
  ( PingStatus
      ( ..,
        PingStatus_ConnectionLost,
        PingStatus_Inactive,
        PingStatus_Online
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PingStatus = PingStatus'
  { fromPingStatus ::
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

pattern PingStatus_ConnectionLost :: PingStatus
pattern PingStatus_ConnectionLost = PingStatus' "ConnectionLost"

pattern PingStatus_Inactive :: PingStatus
pattern PingStatus_Inactive = PingStatus' "Inactive"

pattern PingStatus_Online :: PingStatus
pattern PingStatus_Online = PingStatus' "Online"

{-# COMPLETE
  PingStatus_ConnectionLost,
  PingStatus_Inactive,
  PingStatus_Online,
  PingStatus'
  #-}
