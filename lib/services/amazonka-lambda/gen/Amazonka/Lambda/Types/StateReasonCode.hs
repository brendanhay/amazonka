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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.StateReasonCode
  ( StateReasonCode
      ( ..,
        StateReasonCode_Creating,
        StateReasonCode_DisabledKMSKey,
        StateReasonCode_EFSIOError,
        StateReasonCode_EFSMountConnectivityError,
        StateReasonCode_EFSMountFailure,
        StateReasonCode_EFSMountTimeout,
        StateReasonCode_EniLimitExceeded,
        StateReasonCode_FunctionError,
        StateReasonCode_Idle,
        StateReasonCode_ImageAccessDenied,
        StateReasonCode_ImageDeleted,
        StateReasonCode_InsufficientRolePermissions,
        StateReasonCode_InternalError,
        StateReasonCode_InvalidConfiguration,
        StateReasonCode_InvalidImage,
        StateReasonCode_InvalidRuntime,
        StateReasonCode_InvalidSecurityGroup,
        StateReasonCode_InvalidStateKMSKey,
        StateReasonCode_InvalidSubnet,
        StateReasonCode_InvalidZipFileException,
        StateReasonCode_KMSKeyAccessDenied,
        StateReasonCode_KMSKeyNotFound,
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

pattern StateReasonCode_DisabledKMSKey :: StateReasonCode
pattern StateReasonCode_DisabledKMSKey = StateReasonCode' "DisabledKMSKey"

pattern StateReasonCode_EFSIOError :: StateReasonCode
pattern StateReasonCode_EFSIOError = StateReasonCode' "EFSIOError"

pattern StateReasonCode_EFSMountConnectivityError :: StateReasonCode
pattern StateReasonCode_EFSMountConnectivityError = StateReasonCode' "EFSMountConnectivityError"

pattern StateReasonCode_EFSMountFailure :: StateReasonCode
pattern StateReasonCode_EFSMountFailure = StateReasonCode' "EFSMountFailure"

pattern StateReasonCode_EFSMountTimeout :: StateReasonCode
pattern StateReasonCode_EFSMountTimeout = StateReasonCode' "EFSMountTimeout"

pattern StateReasonCode_EniLimitExceeded :: StateReasonCode
pattern StateReasonCode_EniLimitExceeded = StateReasonCode' "EniLimitExceeded"

pattern StateReasonCode_FunctionError :: StateReasonCode
pattern StateReasonCode_FunctionError = StateReasonCode' "FunctionError"

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

pattern StateReasonCode_InvalidRuntime :: StateReasonCode
pattern StateReasonCode_InvalidRuntime = StateReasonCode' "InvalidRuntime"

pattern StateReasonCode_InvalidSecurityGroup :: StateReasonCode
pattern StateReasonCode_InvalidSecurityGroup = StateReasonCode' "InvalidSecurityGroup"

pattern StateReasonCode_InvalidStateKMSKey :: StateReasonCode
pattern StateReasonCode_InvalidStateKMSKey = StateReasonCode' "InvalidStateKMSKey"

pattern StateReasonCode_InvalidSubnet :: StateReasonCode
pattern StateReasonCode_InvalidSubnet = StateReasonCode' "InvalidSubnet"

pattern StateReasonCode_InvalidZipFileException :: StateReasonCode
pattern StateReasonCode_InvalidZipFileException = StateReasonCode' "InvalidZipFileException"

pattern StateReasonCode_KMSKeyAccessDenied :: StateReasonCode
pattern StateReasonCode_KMSKeyAccessDenied = StateReasonCode' "KMSKeyAccessDenied"

pattern StateReasonCode_KMSKeyNotFound :: StateReasonCode
pattern StateReasonCode_KMSKeyNotFound = StateReasonCode' "KMSKeyNotFound"

pattern StateReasonCode_Restoring :: StateReasonCode
pattern StateReasonCode_Restoring = StateReasonCode' "Restoring"

pattern StateReasonCode_SubnetOutOfIPAddresses :: StateReasonCode
pattern StateReasonCode_SubnetOutOfIPAddresses = StateReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  StateReasonCode_Creating,
  StateReasonCode_DisabledKMSKey,
  StateReasonCode_EFSIOError,
  StateReasonCode_EFSMountConnectivityError,
  StateReasonCode_EFSMountFailure,
  StateReasonCode_EFSMountTimeout,
  StateReasonCode_EniLimitExceeded,
  StateReasonCode_FunctionError,
  StateReasonCode_Idle,
  StateReasonCode_ImageAccessDenied,
  StateReasonCode_ImageDeleted,
  StateReasonCode_InsufficientRolePermissions,
  StateReasonCode_InternalError,
  StateReasonCode_InvalidConfiguration,
  StateReasonCode_InvalidImage,
  StateReasonCode_InvalidRuntime,
  StateReasonCode_InvalidSecurityGroup,
  StateReasonCode_InvalidStateKMSKey,
  StateReasonCode_InvalidSubnet,
  StateReasonCode_InvalidZipFileException,
  StateReasonCode_KMSKeyAccessDenied,
  StateReasonCode_KMSKeyNotFound,
  StateReasonCode_Restoring,
  StateReasonCode_SubnetOutOfIPAddresses,
  StateReasonCode'
  #-}
