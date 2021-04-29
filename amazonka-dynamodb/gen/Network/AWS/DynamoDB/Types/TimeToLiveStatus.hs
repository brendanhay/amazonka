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
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveStatus
  ( TimeToLiveStatus
      ( ..,
        TimeToLiveStatus_DISABLED,
        TimeToLiveStatus_DISABLING,
        TimeToLiveStatus_ENABLED,
        TimeToLiveStatus_ENABLING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TimeToLiveStatus = TimeToLiveStatus'
  { fromTimeToLiveStatus ::
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
