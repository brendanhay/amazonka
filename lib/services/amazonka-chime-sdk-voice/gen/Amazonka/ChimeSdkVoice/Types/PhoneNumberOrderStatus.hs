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
-- Module      : Amazonka.ChimeSdkVoice.Types.PhoneNumberOrderStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.PhoneNumberOrderStatus
  ( PhoneNumberOrderStatus
      ( ..,
        PhoneNumberOrderStatus_CancelRequested,
        PhoneNumberOrderStatus_Cancelled,
        PhoneNumberOrderStatus_ChangeRequested,
        PhoneNumberOrderStatus_Exception,
        PhoneNumberOrderStatus_FOC,
        PhoneNumberOrderStatus_Failed,
        PhoneNumberOrderStatus_Partial,
        PhoneNumberOrderStatus_PendingDocuments,
        PhoneNumberOrderStatus_Processing,
        PhoneNumberOrderStatus_Submitted,
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

pattern PhoneNumberOrderStatus_CancelRequested :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_CancelRequested = PhoneNumberOrderStatus' "CancelRequested"

pattern PhoneNumberOrderStatus_Cancelled :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Cancelled = PhoneNumberOrderStatus' "Cancelled"

pattern PhoneNumberOrderStatus_ChangeRequested :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_ChangeRequested = PhoneNumberOrderStatus' "ChangeRequested"

pattern PhoneNumberOrderStatus_Exception :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Exception = PhoneNumberOrderStatus' "Exception"

pattern PhoneNumberOrderStatus_FOC :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_FOC = PhoneNumberOrderStatus' "FOC"

pattern PhoneNumberOrderStatus_Failed :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Failed = PhoneNumberOrderStatus' "Failed"

pattern PhoneNumberOrderStatus_Partial :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Partial = PhoneNumberOrderStatus' "Partial"

pattern PhoneNumberOrderStatus_PendingDocuments :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_PendingDocuments = PhoneNumberOrderStatus' "PendingDocuments"

pattern PhoneNumberOrderStatus_Processing :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Processing = PhoneNumberOrderStatus' "Processing"

pattern PhoneNumberOrderStatus_Submitted :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Submitted = PhoneNumberOrderStatus' "Submitted"

pattern PhoneNumberOrderStatus_Successful :: PhoneNumberOrderStatus
pattern PhoneNumberOrderStatus_Successful = PhoneNumberOrderStatus' "Successful"

{-# COMPLETE
  PhoneNumberOrderStatus_CancelRequested,
  PhoneNumberOrderStatus_Cancelled,
  PhoneNumberOrderStatus_ChangeRequested,
  PhoneNumberOrderStatus_Exception,
  PhoneNumberOrderStatus_FOC,
  PhoneNumberOrderStatus_Failed,
  PhoneNumberOrderStatus_Partial,
  PhoneNumberOrderStatus_PendingDocuments,
  PhoneNumberOrderStatus_Processing,
  PhoneNumberOrderStatus_Submitted,
  PhoneNumberOrderStatus_Successful,
  PhoneNumberOrderStatus'
  #-}
