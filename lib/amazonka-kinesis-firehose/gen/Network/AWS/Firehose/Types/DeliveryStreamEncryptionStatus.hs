{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
  ( DeliveryStreamEncryptionStatus
    ( DeliveryStreamEncryptionStatus'
    , DeliveryStreamEncryptionStatusEnabled
    , DeliveryStreamEncryptionStatusEnabling
    , DeliveryStreamEncryptionStatusEnablingFailed
    , DeliveryStreamEncryptionStatusDisabled
    , DeliveryStreamEncryptionStatusDisabling
    , DeliveryStreamEncryptionStatusDisablingFailed
    , fromDeliveryStreamEncryptionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeliveryStreamEncryptionStatus = DeliveryStreamEncryptionStatus'{fromDeliveryStreamEncryptionStatus
                                                                         :: Core.Text}
                                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                           Core.Generic)
                                           deriving newtype (Core.IsString, Core.Hashable,
                                                             Core.NFData, Core.ToJSONKey,
                                                             Core.FromJSONKey, Core.ToJSON,
                                                             Core.FromJSON, Core.ToXML,
                                                             Core.FromXML, Core.ToText,
                                                             Core.FromText, Core.ToByteString,
                                                             Core.ToQuery, Core.ToHeader)

pattern DeliveryStreamEncryptionStatusEnabled :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatusEnabled = DeliveryStreamEncryptionStatus' "ENABLED"

pattern DeliveryStreamEncryptionStatusEnabling :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatusEnabling = DeliveryStreamEncryptionStatus' "ENABLING"

pattern DeliveryStreamEncryptionStatusEnablingFailed :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatusEnablingFailed = DeliveryStreamEncryptionStatus' "ENABLING_FAILED"

pattern DeliveryStreamEncryptionStatusDisabled :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatusDisabled = DeliveryStreamEncryptionStatus' "DISABLED"

pattern DeliveryStreamEncryptionStatusDisabling :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatusDisabling = DeliveryStreamEncryptionStatus' "DISABLING"

pattern DeliveryStreamEncryptionStatusDisablingFailed :: DeliveryStreamEncryptionStatus
pattern DeliveryStreamEncryptionStatusDisablingFailed = DeliveryStreamEncryptionStatus' "DISABLING_FAILED"

{-# COMPLETE 
  DeliveryStreamEncryptionStatusEnabled,

  DeliveryStreamEncryptionStatusEnabling,

  DeliveryStreamEncryptionStatusEnablingFailed,

  DeliveryStreamEncryptionStatusDisabled,

  DeliveryStreamEncryptionStatusDisabling,

  DeliveryStreamEncryptionStatusDisablingFailed,
  DeliveryStreamEncryptionStatus'
  #-}
