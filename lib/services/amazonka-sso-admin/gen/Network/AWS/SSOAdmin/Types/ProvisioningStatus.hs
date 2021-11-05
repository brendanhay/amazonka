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
-- Module      : Network.AWS.SSOAdmin.Types.ProvisioningStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSOAdmin.Types.ProvisioningStatus
  ( ProvisioningStatus
      ( ..,
        ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED,
        ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProvisioningStatus = ProvisioningStatus'
  { fromProvisioningStatus ::
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

pattern ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED :: ProvisioningStatus
pattern ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED = ProvisioningStatus' "LATEST_PERMISSION_SET_NOT_PROVISIONED"

pattern ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED :: ProvisioningStatus
pattern ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED = ProvisioningStatus' "LATEST_PERMISSION_SET_PROVISIONED"

{-# COMPLETE
  ProvisioningStatus_LATEST_PERMISSION_SET_NOT_PROVISIONED,
  ProvisioningStatus_LATEST_PERMISSION_SET_PROVISIONED,
  ProvisioningStatus'
  #-}
