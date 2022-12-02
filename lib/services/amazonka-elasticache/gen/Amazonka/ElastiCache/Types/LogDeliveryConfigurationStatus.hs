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
-- Module      : Amazonka.ElastiCache.Types.LogDeliveryConfigurationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.LogDeliveryConfigurationStatus
  ( LogDeliveryConfigurationStatus
      ( ..,
        LogDeliveryConfigurationStatus_Active,
        LogDeliveryConfigurationStatus_Disabling,
        LogDeliveryConfigurationStatus_Enabling,
        LogDeliveryConfigurationStatus_Error,
        LogDeliveryConfigurationStatus_Modifying
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LogDeliveryConfigurationStatus = LogDeliveryConfigurationStatus'
  { fromLogDeliveryConfigurationStatus ::
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

pattern LogDeliveryConfigurationStatus_Active :: LogDeliveryConfigurationStatus
pattern LogDeliveryConfigurationStatus_Active = LogDeliveryConfigurationStatus' "active"

pattern LogDeliveryConfigurationStatus_Disabling :: LogDeliveryConfigurationStatus
pattern LogDeliveryConfigurationStatus_Disabling = LogDeliveryConfigurationStatus' "disabling"

pattern LogDeliveryConfigurationStatus_Enabling :: LogDeliveryConfigurationStatus
pattern LogDeliveryConfigurationStatus_Enabling = LogDeliveryConfigurationStatus' "enabling"

pattern LogDeliveryConfigurationStatus_Error :: LogDeliveryConfigurationStatus
pattern LogDeliveryConfigurationStatus_Error = LogDeliveryConfigurationStatus' "error"

pattern LogDeliveryConfigurationStatus_Modifying :: LogDeliveryConfigurationStatus
pattern LogDeliveryConfigurationStatus_Modifying = LogDeliveryConfigurationStatus' "modifying"

{-# COMPLETE
  LogDeliveryConfigurationStatus_Active,
  LogDeliveryConfigurationStatus_Disabling,
  LogDeliveryConfigurationStatus_Enabling,
  LogDeliveryConfigurationStatus_Error,
  LogDeliveryConfigurationStatus_Modifying,
  LogDeliveryConfigurationStatus'
  #-}
