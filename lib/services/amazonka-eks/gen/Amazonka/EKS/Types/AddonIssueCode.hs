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
-- Module      : Amazonka.EKS.Types.AddonIssueCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonIssueCode
  ( AddonIssueCode
      ( ..,
        AddonIssueCode_AccessDenied,
        AddonIssueCode_AdmissionRequestDenied,
        AddonIssueCode_ClusterUnreachable,
        AddonIssueCode_ConfigurationConflict,
        AddonIssueCode_InsufficientNumberOfReplicas,
        AddonIssueCode_InternalFailure,
        AddonIssueCode_K8sResourceNotFound,
        AddonIssueCode_UnsupportedAddonModification
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AddonIssueCode = AddonIssueCode'
  { fromAddonIssueCode ::
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

pattern AddonIssueCode_AccessDenied :: AddonIssueCode
pattern AddonIssueCode_AccessDenied = AddonIssueCode' "AccessDenied"

pattern AddonIssueCode_AdmissionRequestDenied :: AddonIssueCode
pattern AddonIssueCode_AdmissionRequestDenied = AddonIssueCode' "AdmissionRequestDenied"

pattern AddonIssueCode_ClusterUnreachable :: AddonIssueCode
pattern AddonIssueCode_ClusterUnreachable = AddonIssueCode' "ClusterUnreachable"

pattern AddonIssueCode_ConfigurationConflict :: AddonIssueCode
pattern AddonIssueCode_ConfigurationConflict = AddonIssueCode' "ConfigurationConflict"

pattern AddonIssueCode_InsufficientNumberOfReplicas :: AddonIssueCode
pattern AddonIssueCode_InsufficientNumberOfReplicas = AddonIssueCode' "InsufficientNumberOfReplicas"

pattern AddonIssueCode_InternalFailure :: AddonIssueCode
pattern AddonIssueCode_InternalFailure = AddonIssueCode' "InternalFailure"

pattern AddonIssueCode_K8sResourceNotFound :: AddonIssueCode
pattern AddonIssueCode_K8sResourceNotFound = AddonIssueCode' "K8sResourceNotFound"

pattern AddonIssueCode_UnsupportedAddonModification :: AddonIssueCode
pattern AddonIssueCode_UnsupportedAddonModification = AddonIssueCode' "UnsupportedAddonModification"

{-# COMPLETE
  AddonIssueCode_AccessDenied,
  AddonIssueCode_AdmissionRequestDenied,
  AddonIssueCode_ClusterUnreachable,
  AddonIssueCode_ConfigurationConflict,
  AddonIssueCode_InsufficientNumberOfReplicas,
  AddonIssueCode_InternalFailure,
  AddonIssueCode_K8sResourceNotFound,
  AddonIssueCode_UnsupportedAddonModification,
  AddonIssueCode'
  #-}
