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
-- Module      : Amazonka.DynamoDB.Types.SSEStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.SSEStatus
  ( SSEStatus
      ( ..,
        SSEStatus_DISABLED,
        SSEStatus_DISABLING,
        SSEStatus_ENABLED,
        SSEStatus_ENABLING,
        SSEStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype SSEStatus = SSEStatus'
  { fromSSEStatus ::
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

pattern SSEStatus_DISABLED :: SSEStatus
pattern SSEStatus_DISABLED = SSEStatus' "DISABLED"

pattern SSEStatus_DISABLING :: SSEStatus
pattern SSEStatus_DISABLING = SSEStatus' "DISABLING"

pattern SSEStatus_ENABLED :: SSEStatus
pattern SSEStatus_ENABLED = SSEStatus' "ENABLED"

pattern SSEStatus_ENABLING :: SSEStatus
pattern SSEStatus_ENABLING = SSEStatus' "ENABLING"

pattern SSEStatus_UPDATING :: SSEStatus
pattern SSEStatus_UPDATING = SSEStatus' "UPDATING"

{-# COMPLETE
  SSEStatus_DISABLED,
  SSEStatus_DISABLING,
  SSEStatus_ENABLED,
  SSEStatus_ENABLING,
  SSEStatus_UPDATING,
  SSEStatus'
  #-}
