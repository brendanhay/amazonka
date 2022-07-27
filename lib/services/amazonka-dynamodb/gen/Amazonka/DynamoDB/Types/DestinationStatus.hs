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
-- Module      : Amazonka.DynamoDB.Types.DestinationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.DestinationStatus
  ( DestinationStatus
      ( ..,
        DestinationStatus_ACTIVE,
        DestinationStatus_DISABLED,
        DestinationStatus_DISABLING,
        DestinationStatus_ENABLE_FAILED,
        DestinationStatus_ENABLING
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype DestinationStatus = DestinationStatus'
  { fromDestinationStatus ::
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

pattern DestinationStatus_ACTIVE :: DestinationStatus
pattern DestinationStatus_ACTIVE = DestinationStatus' "ACTIVE"

pattern DestinationStatus_DISABLED :: DestinationStatus
pattern DestinationStatus_DISABLED = DestinationStatus' "DISABLED"

pattern DestinationStatus_DISABLING :: DestinationStatus
pattern DestinationStatus_DISABLING = DestinationStatus' "DISABLING"

pattern DestinationStatus_ENABLE_FAILED :: DestinationStatus
pattern DestinationStatus_ENABLE_FAILED = DestinationStatus' "ENABLE_FAILED"

pattern DestinationStatus_ENABLING :: DestinationStatus
pattern DestinationStatus_ENABLING = DestinationStatus' "ENABLING"

{-# COMPLETE
  DestinationStatus_ACTIVE,
  DestinationStatus_DISABLED,
  DestinationStatus_DISABLING,
  DestinationStatus_ENABLE_FAILED,
  DestinationStatus_ENABLING,
  DestinationStatus'
  #-}
