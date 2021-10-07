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
-- Module      : Network.AWS.Lambda.Types.StateReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.StateReasonCode
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StateReasonCode = StateReasonCode'
  { fromStateReasonCode ::
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
