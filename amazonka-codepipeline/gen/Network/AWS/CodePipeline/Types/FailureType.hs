{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype FailureType = FailureType'
  { fromFailureType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
