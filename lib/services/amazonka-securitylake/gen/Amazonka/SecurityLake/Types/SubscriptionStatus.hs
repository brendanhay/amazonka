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
-- Module      : Amazonka.SecurityLake.Types.SubscriptionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SubscriptionStatus
  ( SubscriptionStatus
      ( ..,
        SubscriptionStatus_ACTIVE,
        SubscriptionStatus_DEACTIVATED,
        SubscriptionStatus_PENDING,
        SubscriptionStatus_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SubscriptionStatus = SubscriptionStatus'
  { fromSubscriptionStatus ::
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

pattern SubscriptionStatus_ACTIVE :: SubscriptionStatus
pattern SubscriptionStatus_ACTIVE = SubscriptionStatus' "ACTIVE"

pattern SubscriptionStatus_DEACTIVATED :: SubscriptionStatus
pattern SubscriptionStatus_DEACTIVATED = SubscriptionStatus' "DEACTIVATED"

pattern SubscriptionStatus_PENDING :: SubscriptionStatus
pattern SubscriptionStatus_PENDING = SubscriptionStatus' "PENDING"

pattern SubscriptionStatus_READY :: SubscriptionStatus
pattern SubscriptionStatus_READY = SubscriptionStatus' "READY"

{-# COMPLETE
  SubscriptionStatus_ACTIVE,
  SubscriptionStatus_DEACTIVATED,
  SubscriptionStatus_PENDING,
  SubscriptionStatus_READY,
  SubscriptionStatus'
  #-}
