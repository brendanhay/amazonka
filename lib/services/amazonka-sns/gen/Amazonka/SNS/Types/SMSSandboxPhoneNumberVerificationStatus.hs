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
-- Module      : Amazonka.SNS.Types.SMSSandboxPhoneNumberVerificationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.SMSSandboxPhoneNumberVerificationStatus
  ( SMSSandboxPhoneNumberVerificationStatus
      ( ..,
        SMSSandboxPhoneNumberVerificationStatus_Pending,
        SMSSandboxPhoneNumberVerificationStatus_Verified
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enum listing out all supported destination phone number verification
-- statuses. The following enum values are supported. 1. PENDING : The
-- destination phone number is pending verification. 2. VERIFIED : The
-- destination phone number is verified.
newtype SMSSandboxPhoneNumberVerificationStatus = SMSSandboxPhoneNumberVerificationStatus'
  { fromSMSSandboxPhoneNumberVerificationStatus ::
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

pattern SMSSandboxPhoneNumberVerificationStatus_Pending :: SMSSandboxPhoneNumberVerificationStatus
pattern SMSSandboxPhoneNumberVerificationStatus_Pending = SMSSandboxPhoneNumberVerificationStatus' "Pending"

pattern SMSSandboxPhoneNumberVerificationStatus_Verified :: SMSSandboxPhoneNumberVerificationStatus
pattern SMSSandboxPhoneNumberVerificationStatus_Verified = SMSSandboxPhoneNumberVerificationStatus' "Verified"

{-# COMPLETE
  SMSSandboxPhoneNumberVerificationStatus_Pending,
  SMSSandboxPhoneNumberVerificationStatus_Verified,
  SMSSandboxPhoneNumberVerificationStatus'
  #-}
