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
-- Module      : Network.AWS.Pinpoint.Types.DeliveryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DeliveryStatus
  ( DeliveryStatus
      ( ..,
        DeliveryStatus_DUPLICATE,
        DeliveryStatus_OPT_OUT,
        DeliveryStatus_PERMANENT_FAILURE,
        DeliveryStatus_SUCCESSFUL,
        DeliveryStatus_TEMPORARY_FAILURE,
        DeliveryStatus_THROTTLED,
        DeliveryStatus_UNKNOWN_FAILURE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeliveryStatus = DeliveryStatus'
  { fromDeliveryStatus ::
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

pattern DeliveryStatus_DUPLICATE :: DeliveryStatus
pattern DeliveryStatus_DUPLICATE = DeliveryStatus' "DUPLICATE"

pattern DeliveryStatus_OPT_OUT :: DeliveryStatus
pattern DeliveryStatus_OPT_OUT = DeliveryStatus' "OPT_OUT"

pattern DeliveryStatus_PERMANENT_FAILURE :: DeliveryStatus
pattern DeliveryStatus_PERMANENT_FAILURE = DeliveryStatus' "PERMANENT_FAILURE"

pattern DeliveryStatus_SUCCESSFUL :: DeliveryStatus
pattern DeliveryStatus_SUCCESSFUL = DeliveryStatus' "SUCCESSFUL"

pattern DeliveryStatus_TEMPORARY_FAILURE :: DeliveryStatus
pattern DeliveryStatus_TEMPORARY_FAILURE = DeliveryStatus' "TEMPORARY_FAILURE"

pattern DeliveryStatus_THROTTLED :: DeliveryStatus
pattern DeliveryStatus_THROTTLED = DeliveryStatus' "THROTTLED"

pattern DeliveryStatus_UNKNOWN_FAILURE :: DeliveryStatus
pattern DeliveryStatus_UNKNOWN_FAILURE = DeliveryStatus' "UNKNOWN_FAILURE"

{-# COMPLETE
  DeliveryStatus_DUPLICATE,
  DeliveryStatus_OPT_OUT,
  DeliveryStatus_PERMANENT_FAILURE,
  DeliveryStatus_SUCCESSFUL,
  DeliveryStatus_TEMPORARY_FAILURE,
  DeliveryStatus_THROTTLED,
  DeliveryStatus_UNKNOWN_FAILURE,
  DeliveryStatus'
  #-}
