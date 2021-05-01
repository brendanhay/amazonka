{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype SSEStatus = SSEStatus'
  { fromSSEStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
