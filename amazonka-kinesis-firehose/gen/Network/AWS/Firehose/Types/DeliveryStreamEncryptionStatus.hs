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
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
  ( DeliveryStreamEncryptionStatus
      ( ..,
        DeliveryStreamEncryptionStatus_DISABLED,
        DeliveryStreamEncryptionStatus_DISABLING,
        DeliveryStreamEncryptionStatus_DISABLING_FAILED,
        DeliveryStreamEncryptionStatus_ENABLED,
        DeliveryStreamEncryptionStatus_ENABLING,
        DeliveryStreamEncryptionStatus_ENABLING_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeliveryStreamEncryptionStatus = DeliveryStreamEncryptionStatus'
  { fromDeliveryStreamEncryptionStatus ::
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

pattern DeliveryStreamEncryptionStatus_DISABLED :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatus_DISABLED = DeliveryStreamEncryptionStatus' "DISABLED"

pattern DeliveryStreamEncryptionStatus_DISABLING :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatus_DISABLING = DeliveryStreamEncryptionStatus' "DISABLING"

pattern DeliveryStreamEncryptionStatus_DISABLING_FAILED :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatus_DISABLING_FAILED = DeliveryStreamEncryptionStatus' "DISABLING_FAILED"

pattern DeliveryStreamEncryptionStatus_ENABLED :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatus_ENABLED = DeliveryStreamEncryptionStatus' "ENABLED"

pattern DeliveryStreamEncryptionStatus_ENABLING :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatus_ENABLING = DeliveryStreamEncryptionStatus' "ENABLING"

pattern DeliveryStreamEncryptionStatus_ENABLING_FAILED :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatus_ENABLING_FAILED = DeliveryStreamEncryptionStatus' "ENABLING_FAILED"

{-# COMPLETE
  DeliveryStreamEncryptionStatus_DISABLED,
  DeliveryStreamEncryptionStatus_DISABLING,
  DeliveryStreamEncryptionStatus_DISABLING_FAILED,
  DeliveryStreamEncryptionStatus_ENABLED,
  DeliveryStreamEncryptionStatus_ENABLING,
  DeliveryStreamEncryptionStatus_ENABLING_FAILED,
  DeliveryStreamEncryptionStatus'
  #-}
