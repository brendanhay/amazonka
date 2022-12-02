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
-- Module      : Amazonka.CodePipeline.Types.FailureType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.FailureType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailureType = FailureType'
  { fromFailureType ::
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
