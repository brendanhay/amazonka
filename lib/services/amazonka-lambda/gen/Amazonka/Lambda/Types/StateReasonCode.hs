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
-- Module      : Amazonka.Lambda.Types.StateReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.StateReasonCode
  ( StateReasonCode
      ( ..,
        StateReasonCode_Creating,
        StateReasonCode_EniLimitExceeded,
        StateReasonCode_Idle,
        StateReasonCode_ImageAccessDenied,
        StateReasonCode_ImageDeleted,
        StateReasonCode_InsufficientRolePermissions,
        StateReasonCode_InternalError,
        StateReasonCode_InvalidConfiguration,
        StateReasonCode_InvalidImage,
        StateReasonCode_InvalidSecurityGroup,
        StateReasonCode_InvalidSubnet,
        StateReasonCode_Restoring,
        StateReasonCode_SubnetOutOfIPAddresses
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StateReasonCode = StateReasonCode'
  { fromStateReasonCode ::
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

pattern StateReasonCode_Creating :: StateReasonCode
pattern StateReasonCode_Creating = StateReasonCode' "Creating"

pattern StateReasonCode_EniLimitExceeded :: StateReasonCode
pattern StateReasonCode_EniLimitExceeded = StateReasonCode' "EniLimitExceeded"

pattern StateReasonCode_Idle :: StateReasonCode
pattern StateReasonCode_Idle = StateReasonCode' "Idle"

pattern StateReasonCode_ImageAccessDenied :: StateReasonCode
pattern StateReasonCode_ImageAccessDenied = StateReasonCode' "ImageAccessDenied"

pattern StateReasonCode_ImageDeleted :: StateReasonCode
pattern StateReasonCode_ImageDeleted = StateReasonCode' "ImageDeleted"

pattern StateReasonCode_InsufficientRolePermissions :: StateReasonCode
pattern StateReasonCode_InsufficientRolePermissions = StateReasonCode' "InsufficientRolePermissions"

pattern StateReasonCode_InternalError :: StateReasonCode
pattern StateReasonCode_InternalError = StateReasonCode' "InternalError"

pattern StateReasonCode_InvalidConfiguration :: StateReasonCode
pattern StateReasonCode_InvalidConfiguration = StateReasonCode' "InvalidConfiguration"

pattern StateReasonCode_InvalidImage :: StateReasonCode
pattern StateReasonCode_InvalidImage = StateReasonCode' "InvalidImage"

pattern StateReasonCode_InvalidSecurityGroup :: StateReasonCode
pattern StateReasonCode_InvalidSecurityGroup = StateReasonCode' "InvalidSecurityGroup"

pattern StateReasonCode_InvalidSubnet :: StateReasonCode
pattern StateReasonCode_InvalidSubnet = StateReasonCode' "InvalidSubnet"

pattern StateReasonCode_Restoring :: StateReasonCode
pattern StateReasonCode_Restoring = StateReasonCode' "Restoring"

pattern StateReasonCode_SubnetOutOfIPAddresses :: StateReasonCode
pattern StateReasonCode_SubnetOutOfIPAddresses = StateReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  StateReasonCode_Creating,
  StateReasonCode_EniLimitExceeded,
  StateReasonCode_Idle,
  StateReasonCode_ImageAccessDenied,
  StateReasonCode_ImageDeleted,
  StateReasonCode_InsufficientRolePermissions,
  StateReasonCode_InternalError,
  StateReasonCode_InvalidConfiguration,
  StateReasonCode_InvalidImage,
  StateReasonCode_InvalidSecurityGroup,
  StateReasonCode_InvalidSubnet,
  StateReasonCode_Restoring,
  StateReasonCode_SubnetOutOfIPAddresses,
  StateReasonCode'
  #-}
