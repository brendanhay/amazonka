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
-- Module      : Amazonka.Chime.Types.PhoneNumberOrderStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberOrderStatus
  ( PhoneNumberOrderStatus
      ( ..,
        PhoneNumberOrderStatus_Failed,
        PhoneNumberOrderStatus_Partial,
        PhoneNumberOrderStatus_Processing,
        PhoneNumberOrderStatus_Successful
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberOrderStatus = PhoneNumberOrderStatus'
  { fromPhoneNumberOrderStatus ::
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
