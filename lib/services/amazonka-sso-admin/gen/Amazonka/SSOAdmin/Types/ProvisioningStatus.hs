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
-- Module      : Amazonka.SSOAdmin.Types.ProvisioningStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.ProvisioningStatus
  ( ProvisioningStatus
      ( ..,
        ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED,
        ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProvisioningStatus = ProvisioningStatus'
  { fromProvisioningStatus ::
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

pattern ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED :: ProvisioningStatus
pattern ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED = ProvisioningStatus' "LATEST_PERMISSION_SET_NOT_PROVISIONED"

pattern ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED :: ProvisioningStatus
pattern ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED = ProvisioningStatus' "LATEST_PERMISSION_SET_PROVISIONED"

{-# COMPLETE
  ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED,
  ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED,
  ProvisioningStatus'
  #-}
