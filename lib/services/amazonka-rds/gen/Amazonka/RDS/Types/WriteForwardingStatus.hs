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
-- Module      : Amazonka.RDS.Types.WriteForwardingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.WriteForwardingStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WriteForwardingStatus = WriteForwardingStatus'
  { fromWriteForwardingStatus ::
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
