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
-- Module      : Amazonka.Route53Domains.Types.ReachabilityStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.ReachabilityStatus
  ( ReachabilityStatus
      ( ..,
        ReachabilityStatus_DONE,
        ReachabilityStatus_EXPIRED,
        ReachabilityStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReachabilityStatus = ReachabilityStatus'
  { fromReachabilityStatus ::
      Data.Text
  }
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
