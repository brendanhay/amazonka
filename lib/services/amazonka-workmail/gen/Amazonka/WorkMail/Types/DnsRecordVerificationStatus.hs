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
-- Module      : Amazonka.WorkMail.Types.DnsRecordVerificationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.DnsRecordVerificationStatus
  ( DnsRecordVerificationStatus
      ( ..,
        DnsRecordVerificationStatus_FAILED,
        DnsRecordVerificationStatus_PENDING,
        DnsRecordVerificationStatus_VERIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DnsRecordVerificationStatus = DnsRecordVerificationStatus'
  { fromDnsRecordVerificationStatus ::
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

pattern DnsRecordVerificationStatus_FAILED :: DnsRecordVerificationStatus
pattern DnsRecordVerificationStatus_FAILED = DnsRecordVerificationStatus' "FAILED"

pattern DnsRecordVerificationStatus_PENDING :: DnsRecordVerificationStatus
pattern DnsRecordVerificationStatus_PENDING = DnsRecordVerificationStatus' "PENDING"

pattern DnsRecordVerificationStatus_VERIFIED :: DnsRecordVerificationStatus
pattern DnsRecordVerificationStatus_VERIFIED = DnsRecordVerificationStatus' "VERIFIED"

{-# COMPLETE
  DnsRecordVerificationStatus_FAILED,
  DnsRecordVerificationStatus_PENDING,
  DnsRecordVerificationStatus_VERIFIED,
  DnsRecordVerificationStatus'
  #-}
