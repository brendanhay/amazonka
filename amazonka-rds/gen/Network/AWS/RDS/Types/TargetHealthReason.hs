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

import qualified Network.AWS.Prelude as Prelude

newtype TargetHealthReason = TargetHealthReason'
  { fromTargetHealthReason ::
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
