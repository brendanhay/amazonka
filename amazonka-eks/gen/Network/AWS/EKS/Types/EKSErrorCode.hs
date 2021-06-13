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
-- Module      : Network.AWS.EKS.Types.EKSErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.EKSErrorCode
  ( EKSErrorCode
      ( ..,
        EKSErrorCode_AccessDenied,
        EKSErrorCode_AdmissionRequestDenied,
        EKSErrorCode_ClusterUnreachable,
        EKSErrorCode_ConfigurationConflict,
        EKSErrorCode_EniLimitReached,
        EKSErrorCode_InsufficientFreeAddresses,
        EKSErrorCode_InsufficientNumberOfReplicas,
        EKSErrorCode_IpNotAvailable,
        EKSErrorCode_NodeCreationFailure,
        EKSErrorCode_OperationNotPermitted,
        EKSErrorCode_PodEvictionFailure,
        EKSErrorCode_SecurityGroupNotFound,
        EKSErrorCode_SubnetNotFound,
        EKSErrorCode_Unknown,
        EKSErrorCode_VpcIdNotFound
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EKSErrorCode = EKSErrorCode'
  { fromEKSErrorCode ::
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

pattern EKSErrorCode_AccessDenied :: EKSErrorCode
pattern EKSErrorCode_AccessDenied = EKSErrorCode' "AccessDenied"

pattern EKSErrorCode_AdmissionRequestDenied :: EKSErrorCode
pattern EKSErrorCode_AdmissionRequestDenied = EKSErrorCode' "AdmissionRequestDenied"

pattern EKSErrorCode_ClusterUnreachable :: EKSErrorCode
pattern EKSErrorCode_ClusterUnreachable = EKSErrorCode' "ClusterUnreachable"

pattern EKSErrorCode_ConfigurationConflict :: EKSErrorCode
pattern EKSErrorCode_ConfigurationConflict = EKSErrorCode' "ConfigurationConflict"

pattern EKSErrorCode_EniLimitReached :: EKSErrorCode
pattern EKSErrorCode_EniLimitReached = EKSErrorCode' "EniLimitReached"

pattern EKSErrorCode_InsufficientFreeAddresses :: EKSErrorCode
pattern EKSErrorCode_InsufficientFreeAddresses = EKSErrorCode' "InsufficientFreeAddresses"

pattern EKSErrorCode_InsufficientNumberOfReplicas :: EKSErrorCode
pattern EKSErrorCode_InsufficientNumberOfReplicas = EKSErrorCode' "InsufficientNumberOfReplicas"

pattern EKSErrorCode_IpNotAvailable :: EKSErrorCode
pattern EKSErrorCode_IpNotAvailable = EKSErrorCode' "IpNotAvailable"

pattern EKSErrorCode_NodeCreationFailure :: EKSErrorCode
pattern EKSErrorCode_NodeCreationFailure = EKSErrorCode' "NodeCreationFailure"

pattern EKSErrorCode_OperationNotPermitted :: EKSErrorCode
pattern EKSErrorCode_OperationNotPermitted = EKSErrorCode' "OperationNotPermitted"

pattern EKSErrorCode_PodEvictionFailure :: EKSErrorCode
pattern EKSErrorCode_PodEvictionFailure = EKSErrorCode' "PodEvictionFailure"

pattern EKSErrorCode_SecurityGroupNotFound :: EKSErrorCode
pattern EKSErrorCode_SecurityGroupNotFound = EKSErrorCode' "SecurityGroupNotFound"

pattern EKSErrorCode_SubnetNotFound :: EKSErrorCode
pattern EKSErrorCode_SubnetNotFound = EKSErrorCode' "SubnetNotFound"

pattern EKSErrorCode_Unknown :: EKSErrorCode
pattern EKSErrorCode_Unknown = EKSErrorCode' "Unknown"

pattern EKSErrorCode_VpcIdNotFound :: EKSErrorCode
pattern EKSErrorCode_VpcIdNotFound = EKSErrorCode' "VpcIdNotFound"

{-# COMPLETE
  EKSErrorCode_AccessDenied,
  EKSErrorCode_AdmissionRequestDenied,
  EKSErrorCode_ClusterUnreachable,
  EKSErrorCode_ConfigurationConflict,
  EKSErrorCode_EniLimitReached,
  EKSErrorCode_InsufficientFreeAddresses,
  EKSErrorCode_InsufficientNumberOfReplicas,
  EKSErrorCode_IpNotAvailable,
  EKSErrorCode_NodeCreationFailure,
  EKSErrorCode_OperationNotPermitted,
  EKSErrorCode_PodEvictionFailure,
  EKSErrorCode_SecurityGroupNotFound,
  EKSErrorCode_SubnetNotFound,
  EKSErrorCode_Unknown,
  EKSErrorCode_VpcIdNotFound,
  EKSErrorCode'
  #-}
