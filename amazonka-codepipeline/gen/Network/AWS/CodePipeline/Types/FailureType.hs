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
-- Module      : Network.AWS.CodePipeline.Types.FailureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureType
  ( FailureType
      ( ..,
        FailureType_ConfigurationError,
        FailureType_JobFailed,
        FailureType_PermissionError,
        FailureType_RevisionOutOfSync,
        FailureType_RevisionUnavailable,
        FailureType_SystemUnavailable
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FailureType = FailureType'
  { fromFailureType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern FailureType_ConfigurationError :: FailureType
pattern FailureType_ConfigurationError = FailureType' "ConfigurationError"

pattern FailureType_JobFailed :: FailureType
pattern FailureType_JobFailed = FailureType' "JobFailed"

pattern FailureType_PermissionError :: FailureType
pattern FailureType_PermissionError = FailureType' "PermissionError"

pattern FailureType_RevisionOutOfSync :: FailureType
pattern FailureType_RevisionOutOfSync = FailureType' "RevisionOutOfSync"

pattern FailureType_RevisionUnavailable :: FailureType
pattern FailureType_RevisionUnavailable = FailureType' "RevisionUnavailable"

pattern FailureType_SystemUnavailable :: FailureType
pattern FailureType_SystemUnavailable = FailureType' "SystemUnavailable"

{-# COMPLETE
  FailureType_ConfigurationError,
  FailureType_JobFailed,
  FailureType_PermissionError,
  FailureType_RevisionOutOfSync,
  FailureType_RevisionUnavailable,
  FailureType_SystemUnavailable,
  FailureType'
  #-}
