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
-- Module      : Network.AWS.Route53Domains.Types.ReachabilityStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ReachabilityStatus
  ( ReachabilityStatus
      ( ..,
        ReachabilityStatus_DONE,
        ReachabilityStatus_EXPIRED,
        ReachabilityStatus_PENDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReachabilityStatus = ReachabilityStatus'
  { fromReachabilityStatus ::
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

pattern ReachabilityStatus_DONE :: ReachabilityStatus
pattern ReachabilityStatus_DONE = ReachabilityStatus' "DONE"

pattern ReachabilityStatus_EXPIRED :: ReachabilityStatus
pattern ReachabilityStatus_EXPIRED = ReachabilityStatus' "EXPIRED"

pattern ReachabilityStatus_PENDING :: ReachabilityStatus
pattern ReachabilityStatus_PENDING = ReachabilityStatus' "PENDING"

{-# COMPLETE
  ReachabilityStatus_DONE,
  ReachabilityStatus_EXPIRED,
  ReachabilityStatus_PENDING,
  ReachabilityStatus'
  #-}
