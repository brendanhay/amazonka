-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.FailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureType
  ( FailureType
      ( FailureType',
        ConfigurationError,
        JobFailed,
        PermissionError,
        RevisionOutOfSync,
        RevisionUnavailable,
        SystemUnavailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FailureType = FailureType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ConfigurationError :: FailureType
pattern ConfigurationError = FailureType' "ConfigurationError"

pattern JobFailed :: FailureType
pattern JobFailed = FailureType' "JobFailed"

pattern PermissionError :: FailureType
pattern PermissionError = FailureType' "PermissionError"

pattern RevisionOutOfSync :: FailureType
pattern RevisionOutOfSync = FailureType' "RevisionOutOfSync"

pattern RevisionUnavailable :: FailureType
pattern RevisionUnavailable = FailureType' "RevisionUnavailable"

pattern SystemUnavailable :: FailureType
pattern SystemUnavailable = FailureType' "SystemUnavailable"

{-# COMPLETE
  ConfigurationError,
  JobFailed,
  PermissionError,
  RevisionOutOfSync,
  RevisionUnavailable,
  SystemUnavailable,
  FailureType'
  #-}
