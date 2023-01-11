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
-- Module      : Amazonka.DynamoDB.Types.TimeToLiveStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TimeToLiveStatus
  ( TimeToLiveStatus
      ( ..,
        TimeToLiveStatus_DISABLED,
        TimeToLiveStatus_DISABLING,
        TimeToLiveStatus_ENABLED,
        TimeToLiveStatus_ENABLING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype TimeToLiveStatus = TimeToLiveStatus'
  { fromTimeToLiveStatus ::
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

pattern TimeToLiveStatus_DISABLED :: TimeToLiveStatus
pattern TimeToLiveStatus_DISABLED = TimeToLiveStatus' "DISABLED"

pattern TimeToLiveStatus_DISABLING :: TimeToLiveStatus
pattern TimeToLiveStatus_DISABLING = TimeToLiveStatus' "DISABLING"

pattern TimeToLiveStatus_ENABLED :: TimeToLiveStatus
pattern TimeToLiveStatus_ENABLED = TimeToLiveStatus' "ENABLED"

pattern TimeToLiveStatus_ENABLING :: TimeToLiveStatus
pattern TimeToLiveStatus_ENABLING = TimeToLiveStatus' "ENABLING"

{-# COMPLETE
  TimeToLiveStatus_DISABLED,
  TimeToLiveStatus_DISABLING,
  TimeToLiveStatus_ENABLED,
  TimeToLiveStatus_ENABLING,
  TimeToLiveStatus'
  #-}
