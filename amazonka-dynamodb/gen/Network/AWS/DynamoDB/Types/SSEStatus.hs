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
-- Module      : Network.AWS.DynamoDB.Types.SSEStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SSEStatus = SSEStatus'
  { fromSSEStatus ::
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
