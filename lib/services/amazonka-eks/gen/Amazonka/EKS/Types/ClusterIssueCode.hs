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
-- Module      : Amazonka.EKS.Types.ClusterIssueCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ClusterIssueCode
  ( ClusterIssueCode
      ( ..,
        ClusterIssueCode_AccessDenied,
        ClusterIssueCode_ClusterUnreachable,
        ClusterIssueCode_ConfigurationConflict,
        ClusterIssueCode_InternalFailure,
        ClusterIssueCode_ResourceLimitExceeded,
        ClusterIssueCode_ResourceNotFound
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClusterIssueCode = ClusterIssueCode'
  { fromClusterIssueCode ::
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

pattern ClusterIssueCode_AccessDenied :: ClusterIssueCode
pattern ClusterIssueCode_AccessDenied = ClusterIssueCode' "AccessDenied"

pattern ClusterIssueCode_ClusterUnreachable :: ClusterIssueCode
pattern ClusterIssueCode_ClusterUnreachable = ClusterIssueCode' "ClusterUnreachable"

pattern ClusterIssueCode_ConfigurationConflict :: ClusterIssueCode
pattern ClusterIssueCode_ConfigurationConflict = ClusterIssueCode' "ConfigurationConflict"

pattern ClusterIssueCode_InternalFailure :: ClusterIssueCode
pattern ClusterIssueCode_InternalFailure = ClusterIssueCode' "InternalFailure"

pattern ClusterIssueCode_ResourceLimitExceeded :: ClusterIssueCode
pattern ClusterIssueCode_ResourceLimitExceeded = ClusterIssueCode' "ResourceLimitExceeded"

pattern ClusterIssueCode_ResourceNotFound :: ClusterIssueCode
pattern ClusterIssueCode_ResourceNotFound = ClusterIssueCode' "ResourceNotFound"

{-# COMPLETE
  ClusterIssueCode_AccessDenied,
  ClusterIssueCode_ClusterUnreachable,
  ClusterIssueCode_ConfigurationConflict,
  ClusterIssueCode_InternalFailure,
  ClusterIssueCode_ResourceLimitExceeded,
  ClusterIssueCode_ResourceNotFound,
  ClusterIssueCode'
  #-}
