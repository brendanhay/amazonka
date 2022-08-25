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
-- Module      : Amazonka.Outposts.Types.OrderStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.OrderStatus
  ( OrderStatus
      ( ..,
        OrderStatus_CANCELLED,
        OrderStatus_COMPLETED,
        OrderStatus_ERROR,
        OrderStatus_FULFILLED,
        OrderStatus_INSTALLING,
        OrderStatus_IN_PROGRESS,
        OrderStatus_PENDING,
        OrderStatus_PREPARING,
        OrderStatus_PROCESSING,
        OrderStatus_RECEIVED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OrderStatus = OrderStatus'
  { fromOrderStatus ::
      Core.Text
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

pattern OrderStatus_CANCELLED :: OrderStatus
pattern OrderStatus_CANCELLED = OrderStatus' "CANCELLED"

pattern OrderStatus_COMPLETED :: OrderStatus
pattern OrderStatus_COMPLETED = OrderStatus' "COMPLETED"

pattern OrderStatus_ERROR :: OrderStatus
pattern OrderStatus_ERROR = OrderStatus' "ERROR"

pattern OrderStatus_FULFILLED :: OrderStatus
pattern OrderStatus_FULFILLED = OrderStatus' "FULFILLED"

pattern OrderStatus_INSTALLING :: OrderStatus
pattern OrderStatus_INSTALLING = OrderStatus' "INSTALLING"

pattern OrderStatus_IN_PROGRESS :: OrderStatus
pattern OrderStatus_IN_PROGRESS = OrderStatus' "IN_PROGRESS"

pattern OrderStatus_PENDING :: OrderStatus
pattern OrderStatus_PENDING = OrderStatus' "PENDING"

pattern OrderStatus_PREPARING :: OrderStatus
pattern OrderStatus_PREPARING = OrderStatus' "PREPARING"

pattern OrderStatus_PROCESSING :: OrderStatus
pattern OrderStatus_PROCESSING = OrderStatus' "PROCESSING"

pattern OrderStatus_RECEIVED :: OrderStatus
pattern OrderStatus_RECEIVED = OrderStatus' "RECEIVED"

{-# COMPLETE
  OrderStatus_CANCELLED,
  OrderStatus_COMPLETED,
  OrderStatus_ERROR,
  OrderStatus_FULFILLED,
  OrderStatus_INSTALLING,
  OrderStatus_IN_PROGRESS,
  OrderStatus_PENDING,
  OrderStatus_PREPARING,
  OrderStatus_PROCESSING,
  OrderStatus_RECEIVED,
  OrderStatus'
  #-}
