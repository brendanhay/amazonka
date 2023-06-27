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
-- Module      : Amazonka.SecurityLake.Types.SubscriberStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SubscriberStatus
  ( SubscriberStatus
      ( ..,
        SubscriberStatus_ACTIVE,
        SubscriberStatus_DEACTIVATED,
        SubscriberStatus_PENDING,
        SubscriberStatus_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SubscriberStatus = SubscriberStatus'
  { fromSubscriberStatus ::
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

pattern SubscriberStatus_ACTIVE :: SubscriberStatus
pattern SubscriberStatus_ACTIVE = SubscriberStatus' "ACTIVE"

pattern SubscriberStatus_DEACTIVATED :: SubscriberStatus
pattern SubscriberStatus_DEACTIVATED = SubscriberStatus' "DEACTIVATED"

pattern SubscriberStatus_PENDING :: SubscriberStatus
pattern SubscriberStatus_PENDING = SubscriberStatus' "PENDING"

pattern SubscriberStatus_READY :: SubscriberStatus
pattern SubscriberStatus_READY = SubscriberStatus' "READY"

{-# COMPLETE
  SubscriberStatus_ACTIVE,
  SubscriberStatus_DEACTIVATED,
  SubscriberStatus_PENDING,
  SubscriberStatus_READY,
  SubscriberStatus'
  #-}
