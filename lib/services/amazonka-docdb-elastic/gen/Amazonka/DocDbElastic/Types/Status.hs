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
-- Module      : Amazonka.DocDbElastic.Types.Status
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Types.Status
  ( Status
      ( ..,
        Status_ACTIVE,
        Status_CREATING,
        Status_DELETING,
        Status_INACCESSIBLE_ENCRYPTION_CREDS,
        Status_INVALID_SECURITY_GROUP_ID,
        Status_INVALID_SUBNET_ID,
        Status_IP_ADDRESS_LIMIT_EXCEEDED,
        Status_UPDATING,
        Status_VPC_ENDPOINT_LIMIT_EXCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Status = Status' {fromStatus :: Data.Text}
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

pattern Status_ACTIVE :: Status
pattern Status_ACTIVE = Status' "ACTIVE"

pattern Status_CREATING :: Status
pattern Status_CREATING = Status' "CREATING"

pattern Status_DELETING :: Status
pattern Status_DELETING = Status' "DELETING"

pattern Status_INACCESSIBLE_ENCRYPTION_CREDS :: Status
pattern Status_INACCESSIBLE_ENCRYPTION_CREDS = Status' "INACCESSIBLE_ENCRYPTION_CREDS"

pattern Status_INVALID_SECURITY_GROUP_ID :: Status
pattern Status_INVALID_SECURITY_GROUP_ID = Status' "INVALID_SECURITY_GROUP_ID"

pattern Status_INVALID_SUBNET_ID :: Status
pattern Status_INVALID_SUBNET_ID = Status' "INVALID_SUBNET_ID"

pattern Status_IP_ADDRESS_LIMIT_EXCEEDED :: Status
pattern Status_IP_ADDRESS_LIMIT_EXCEEDED = Status' "IP_ADDRESS_LIMIT_EXCEEDED"

pattern Status_UPDATING :: Status
pattern Status_UPDATING = Status' "UPDATING"

pattern Status_VPC_ENDPOINT_LIMIT_EXCEEDED :: Status
pattern Status_VPC_ENDPOINT_LIMIT_EXCEEDED = Status' "VPC_ENDPOINT_LIMIT_EXCEEDED"

{-# COMPLETE
  Status_ACTIVE,
  Status_CREATING,
  Status_DELETING,
  Status_INACCESSIBLE_ENCRYPTION_CREDS,
  Status_INVALID_SECURITY_GROUP_ID,
  Status_INVALID_SUBNET_ID,
  Status_IP_ADDRESS_LIMIT_EXCEEDED,
  Status_UPDATING,
  Status_VPC_ENDPOINT_LIMIT_EXCEEDED,
  Status'
  #-}
