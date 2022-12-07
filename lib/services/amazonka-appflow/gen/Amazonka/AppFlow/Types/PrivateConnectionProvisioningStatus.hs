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
-- Module      : Amazonka.AppFlow.Types.PrivateConnectionProvisioningStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PrivateConnectionProvisioningStatus
  ( PrivateConnectionProvisioningStatus
      ( ..,
        PrivateConnectionProvisioningStatus_CREATED,
        PrivateConnectionProvisioningStatus_FAILED,
        PrivateConnectionProvisioningStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PrivateConnectionProvisioningStatus = PrivateConnectionProvisioningStatus'
  { fromPrivateConnectionProvisioningStatus ::
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

pattern PrivateConnectionProvisioningStatus_CREATED :: PrivateConnectionProvisioningStatus
pattern PrivateConnectionProvisioningStatus_CREATED = PrivateConnectionProvisioningStatus' "CREATED"

pattern PrivateConnectionProvisioningStatus_FAILED :: PrivateConnectionProvisioningStatus
pattern PrivateConnectionProvisioningStatus_FAILED = PrivateConnectionProvisioningStatus' "FAILED"

pattern PrivateConnectionProvisioningStatus_PENDING :: PrivateConnectionProvisioningStatus
pattern PrivateConnectionProvisioningStatus_PENDING = PrivateConnectionProvisioningStatus' "PENDING"

{-# COMPLETE
  PrivateConnectionProvisioningStatus_CREATED,
  PrivateConnectionProvisioningStatus_FAILED,
  PrivateConnectionProvisioningStatus_PENDING,
  PrivateConnectionProvisioningStatus'
  #-}
