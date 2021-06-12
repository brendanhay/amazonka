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
-- Module      : Network.AWS.Snowball.Types.ShippingLabelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingLabelStatus
  ( ShippingLabelStatus
      ( ..,
        ShippingLabelStatus_Failed,
        ShippingLabelStatus_InProgress,
        ShippingLabelStatus_Succeeded,
        ShippingLabelStatus_TimedOut
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ShippingLabelStatus = ShippingLabelStatus'
  { fromShippingLabelStatus ::
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

pattern ShippingLabelStatus_Failed :: ShippingLabelStatus
pattern ShippingLabelStatus_Failed = ShippingLabelStatus' "Failed"

pattern ShippingLabelStatus_InProgress :: ShippingLabelStatus
pattern ShippingLabelStatus_InProgress = ShippingLabelStatus' "InProgress"

pattern ShippingLabelStatus_Succeeded :: ShippingLabelStatus
pattern ShippingLabelStatus_Succeeded = ShippingLabelStatus' "Succeeded"

pattern ShippingLabelStatus_TimedOut :: ShippingLabelStatus
pattern ShippingLabelStatus_TimedOut = ShippingLabelStatus' "TimedOut"

{-# COMPLETE
  ShippingLabelStatus_Failed,
  ShippingLabelStatus_InProgress,
  ShippingLabelStatus_Succeeded,
  ShippingLabelStatus_TimedOut,
  ShippingLabelStatus'
  #-}
