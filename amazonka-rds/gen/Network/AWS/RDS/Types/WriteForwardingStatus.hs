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
-- Module      : Network.AWS.RDS.Types.WriteForwardingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.WriteForwardingStatus
  ( WriteForwardingStatus
      ( ..,
        WriteForwardingStatus_Disabled,
        WriteForwardingStatus_Disabling,
        WriteForwardingStatus_Enabled,
        WriteForwardingStatus_Enabling,
        WriteForwardingStatus_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype WriteForwardingStatus = WriteForwardingStatus'
  { fromWriteForwardingStatus ::
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

pattern WriteForwardingStatus_Disabled :: WriteForwardingStatus
pattern WriteForwardingStatus_Disabled = WriteForwardingStatus' "disabled"

pattern WriteForwardingStatus_Disabling :: WriteForwardingStatus
pattern WriteForwardingStatus_Disabling = WriteForwardingStatus' "disabling"

pattern WriteForwardingStatus_Enabled :: WriteForwardingStatus
pattern WriteForwardingStatus_Enabled = WriteForwardingStatus' "enabled"

pattern WriteForwardingStatus_Enabling :: WriteForwardingStatus
pattern WriteForwardingStatus_Enabling = WriteForwardingStatus' "enabling"

pattern WriteForwardingStatus_Unknown :: WriteForwardingStatus
pattern WriteForwardingStatus_Unknown = WriteForwardingStatus' "unknown"

{-# COMPLETE
  WriteForwardingStatus_Disabled,
  WriteForwardingStatus_Disabling,
  WriteForwardingStatus_Enabled,
  WriteForwardingStatus_Enabling,
  WriteForwardingStatus_Unknown,
  WriteForwardingStatus'
  #-}
