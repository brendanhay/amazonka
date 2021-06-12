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
-- Module      : Network.AWS.RDS.Types.TargetHealthReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealthReason
  ( TargetHealthReason
      ( ..,
        TargetHealthReason_AUTH_FAILURE,
        TargetHealthReason_CONNECTION_FAILED,
        TargetHealthReason_PENDING_PROXY_CAPACITY,
        TargetHealthReason_UNREACHABLE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TargetHealthReason = TargetHealthReason'
  { fromTargetHealthReason ::
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

pattern TargetHealthReason_AUTH_FAILURE :: TargetHealthReason
pattern TargetHealthReason_AUTH_FAILURE = TargetHealthReason' "AUTH_FAILURE"

pattern TargetHealthReason_CONNECTION_FAILED :: TargetHealthReason
pattern TargetHealthReason_CONNECTION_FAILED = TargetHealthReason' "CONNECTION_FAILED"

pattern TargetHealthReason_PENDING_PROXY_CAPACITY :: TargetHealthReason
pattern TargetHealthReason_PENDING_PROXY_CAPACITY = TargetHealthReason' "PENDING_PROXY_CAPACITY"

pattern TargetHealthReason_UNREACHABLE :: TargetHealthReason
pattern TargetHealthReason_UNREACHABLE = TargetHealthReason' "UNREACHABLE"

{-# COMPLETE
  TargetHealthReason_AUTH_FAILURE,
  TargetHealthReason_CONNECTION_FAILED,
  TargetHealthReason_PENDING_PROXY_CAPACITY,
  TargetHealthReason_UNREACHABLE,
  TargetHealthReason'
  #-}
