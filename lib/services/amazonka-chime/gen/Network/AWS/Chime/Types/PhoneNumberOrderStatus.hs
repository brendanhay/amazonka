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
-- Module      : Network.AWS.Chime.Types.PhoneNumberOrderStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.PhoneNumberOrderStatus
  ( PhoneNumberOrderStatus
      ( ..,
        PhoneNumberOrderStatus_Failed,
        PhoneNumberOrderStatus_Partial,
        PhoneNumberOrderStatus_Processing,
        PhoneNumberOrderStatus_Successful
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PhoneNumberOrderStatus = PhoneNumberOrderStatus'
  { fromPhoneNumberOrderStatus ::
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

pattern PhoneNumberOrderStatus_Failed :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Failed = PhoneNumberOrderStatus' "Failed"

pattern PhoneNumberOrderStatus_Partial :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Partial = PhoneNumberOrderStatus' "Partial"

pattern PhoneNumberOrderStatus_Processing :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Processing = PhoneNumberOrderStatus' "Processing"

pattern PhoneNumberOrderStatus_Successful :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Successful = PhoneNumberOrderStatus' "Successful"

{-# COMPLETE
  PhoneNumberOrderStatus_Failed,
  PhoneNumberOrderStatus_Partial,
  PhoneNumberOrderStatus_Processing,
  PhoneNumberOrderStatus_Successful,
  PhoneNumberOrderStatus'
  #-}
