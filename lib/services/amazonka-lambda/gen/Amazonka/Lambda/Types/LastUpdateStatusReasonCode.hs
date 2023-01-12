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
-- Module      : Amazonka.Lambda.Types.LastUpdateStatusReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.LastUpdateStatusReasonCode
  ( LastUpdateStatusReasonCode
      ( ..,
        LastUpdateStatusReasonCode_DisabledKMSKey,
        LastUpdateStatusReasonCode_EFSIOError,
        LastUpdateStatusReasonCode_EFSMountConnectivityError,
        LastUpdateStatusReasonCode_EFSMountFailure,
        LastUpdateStatusReasonCode_EFSMountTimeout,
        LastUpdateStatusReasonCode_EniLimitExceeded,
        LastUpdateStatusReasonCode_FunctionError,
        LastUpdateStatusReasonCode_ImageAccessDenied,
        LastUpdateStatusReasonCode_ImageDeleted,
        LastUpdateStatusReasonCode_InsufficientRolePermissions,
        LastUpdateStatusReasonCode_InternalError,
        LastUpdateStatusReasonCode_InvalidConfiguration,
        LastUpdateStatusReasonCode_InvalidImage,
        LastUpdateStatusReasonCode_InvalidRuntime,
        LastUpdateStatusReasonCode_InvalidSecurityGroup,
        LastUpdateStatusReasonCode_InvalidStateKMSKey,
        LastUpdateStatusReasonCode_InvalidSubnet,
        LastUpdateStatusReasonCode_InvalidZipFileException,
        LastUpdateStatusReasonCode_KMSKeyAccessDenied,
        LastUpdateStatusReasonCode_KMSKeyNotFound,
        LastUpdateStatusReasonCode_SubnetOutOfIPAddresses
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LastUpdateStatusReasonCode = LastUpdateStatusReasonCode'
  { fromLastUpdateStatusReasonCode ::
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

pattern LastUpdateStatusReasonCode_DisabledKMSKey :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_DisabledKMSKey = LastUpdateStatusReasonCode' "DisabledKMSKey"

pattern LastUpdateStatusReasonCode_EFSIOError :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_EFSIOError = LastUpdateStatusReasonCode' "EFSIOError"

pattern LastUpdateStatusReasonCode_EFSMountConnectivityError :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_EFSMountConnectivityError = LastUpdateStatusReasonCode' "EFSMountConnectivityError"

pattern LastUpdateStatusReasonCode_EFSMountFailure :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_EFSMountFailure = LastUpdateStatusReasonCode' "EFSMountFailure"

pattern LastUpdateStatusReasonCode_EFSMountTimeout :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_EFSMountTimeout = LastUpdateStatusReasonCode' "EFSMountTimeout"

pattern LastUpdateStatusReasonCode_EniLimitExceeded :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_EniLimitExceeded = LastUpdateStatusReasonCode' "EniLimitExceeded"

pattern LastUpdateStatusReasonCode_FunctionError :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_FunctionError = LastUpdateStatusReasonCode' "FunctionError"

pattern LastUpdateStatusReasonCode_ImageAccessDenied :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_ImageAccessDenied = LastUpdateStatusReasonCode' "ImageAccessDenied"

pattern LastUpdateStatusReasonCode_ImageDeleted :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_ImageDeleted = LastUpdateStatusReasonCode' "ImageDeleted"

pattern LastUpdateStatusReasonCode_InsufficientRolePermissions :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InsufficientRolePermissions = LastUpdateStatusReasonCode' "InsufficientRolePermissions"

pattern LastUpdateStatusReasonCode_InternalError :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InternalError = LastUpdateStatusReasonCode' "InternalError"

pattern LastUpdateStatusReasonCode_InvalidConfiguration :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidConfiguration = LastUpdateStatusReasonCode' "InvalidConfiguration"

pattern LastUpdateStatusReasonCode_InvalidImage :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidImage = LastUpdateStatusReasonCode' "InvalidImage"

pattern LastUpdateStatusReasonCode_InvalidRuntime :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidRuntime = LastUpdateStatusReasonCode' "InvalidRuntime"

pattern LastUpdateStatusReasonCode_InvalidSecurityGroup :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidSecurityGroup = LastUpdateStatusReasonCode' "InvalidSecurityGroup"

pattern LastUpdateStatusReasonCode_InvalidStateKMSKey :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidStateKMSKey = LastUpdateStatusReasonCode' "InvalidStateKMSKey"

pattern LastUpdateStatusReasonCode_InvalidSubnet :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidSubnet = LastUpdateStatusReasonCode' "InvalidSubnet"

pattern LastUpdateStatusReasonCode_InvalidZipFileException :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_InvalidZipFileException = LastUpdateStatusReasonCode' "InvalidZipFileException"

pattern LastUpdateStatusReasonCode_KMSKeyAccessDenied :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_KMSKeyAccessDenied = LastUpdateStatusReasonCode' "KMSKeyAccessDenied"

pattern LastUpdateStatusReasonCode_KMSKeyNotFound :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_KMSKeyNotFound = LastUpdateStatusReasonCode' "KMSKeyNotFound"

pattern LastUpdateStatusReasonCode_SubnetOutOfIPAddresses :: LastUpdateStatusReasonCode
pattern LastUpdateStatusReasonCode_SubnetOutOfIPAddresses = LastUpdateStatusReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  LastUpdateStatusReasonCode_DisabledKMSKey,
  LastUpdateStatusReasonCode_EFSIOError,
  LastUpdateStatusReasonCode_EFSMountConnectivityError,
  LastUpdateStatusReasonCode_EFSMountFailure,
  LastUpdateStatusReasonCode_EFSMountTimeout,
  LastUpdateStatusReasonCode_EniLimitExceeded,
  LastUpdateStatusReasonCode_FunctionError,
  LastUpdateStatusReasonCode_ImageAccessDenied,
  LastUpdateStatusReasonCode_ImageDeleted,
  LastUpdateStatusReasonCode_InsufficientRolePermissions,
  LastUpdateStatusReasonCode_InternalError,
  LastUpdateStatusReasonCode_InvalidConfiguration,
  LastUpdateStatusReasonCode_InvalidImage,
  LastUpdateStatusReasonCode_InvalidRuntime,
  LastUpdateStatusReasonCode_InvalidSecurityGroup,
  LastUpdateStatusReasonCode_InvalidStateKMSKey,
  LastUpdateStatusReasonCode_InvalidSubnet,
  LastUpdateStatusReasonCode_InvalidZipFileException,
  LastUpdateStatusReasonCode_KMSKeyAccessDenied,
  LastUpdateStatusReasonCode_KMSKeyNotFound,
  LastUpdateStatusReasonCode_SubnetOutOfIPAddresses,
  LastUpdateStatusReasonCode'
  #-}
