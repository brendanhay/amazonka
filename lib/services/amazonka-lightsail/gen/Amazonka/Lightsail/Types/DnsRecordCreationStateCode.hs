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
-- Module      : Amazonka.Lightsail.Types.DnsRecordCreationStateCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DnsRecordCreationStateCode
  ( DnsRecordCreationStateCode
      ( ..,
        DnsRecordCreationStateCode_FAILED,
        DnsRecordCreationStateCode_STARTED,
        DnsRecordCreationStateCode_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DnsRecordCreationStateCode = DnsRecordCreationStateCode'
  { fromDnsRecordCreationStateCode ::
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

pattern DnsRecordCreationStateCode_FAILED :: DnsRecordCreationStateCode
pattern DnsRecordCreationStateCode_FAILED = DnsRecordCreationStateCode' "FAILED"

pattern DnsRecordCreationStateCode_STARTED :: DnsRecordCreationStateCode
pattern DnsRecordCreationStateCode_STARTED = DnsRecordCreationStateCode' "STARTED"

pattern DnsRecordCreationStateCode_SUCCEEDED :: DnsRecordCreationStateCode
pattern DnsRecordCreationStateCode_SUCCEEDED = DnsRecordCreationStateCode' "SUCCEEDED"

{-# COMPLETE
  DnsRecordCreationStateCode_FAILED,
  DnsRecordCreationStateCode_STARTED,
  DnsRecordCreationStateCode_SUCCEEDED,
  DnsRecordCreationStateCode'
  #-}
