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
-- Module      : Amazonka.FinSpaceData.Types.ErrorCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ErrorCategory
  ( ErrorCategory
      ( ..,
        ErrorCategory_ACCESS_DENIED,
        ErrorCategory_CANCELLED,
        ErrorCategory_INTERNAL_SERVICE_EXCEPTION,
        ErrorCategory_RESOURCE_NOT_FOUND,
        ErrorCategory_SERVICE_QUOTA_EXCEEDED,
        ErrorCategory_THROTTLING,
        ErrorCategory_USER_RECOVERABLE,
        ErrorCategory_VALIDATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Changeset Error Category
newtype ErrorCategory = ErrorCategory'
  { fromErrorCategory ::
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

pattern ErrorCategory_ACCESS_DENIED :: ErrorCategory
pattern ErrorCategory_ACCESS_DENIED = ErrorCategory' "ACCESS_DENIED"

pattern ErrorCategory_CANCELLED :: ErrorCategory
pattern ErrorCategory_CANCELLED = ErrorCategory' "CANCELLED"

pattern ErrorCategory_INTERNAL_SERVICE_EXCEPTION :: ErrorCategory
pattern ErrorCategory_INTERNAL_SERVICE_EXCEPTION = ErrorCategory' "INTERNAL_SERVICE_EXCEPTION"

pattern ErrorCategory_RESOURCE_NOT_FOUND :: ErrorCategory
pattern ErrorCategory_RESOURCE_NOT_FOUND = ErrorCategory' "RESOURCE_NOT_FOUND"

pattern ErrorCategory_SERVICE_QUOTA_EXCEEDED :: ErrorCategory
pattern ErrorCategory_SERVICE_QUOTA_EXCEEDED = ErrorCategory' "SERVICE_QUOTA_EXCEEDED"

pattern ErrorCategory_THROTTLING :: ErrorCategory
pattern ErrorCategory_THROTTLING = ErrorCategory' "THROTTLING"

pattern ErrorCategory_USER_RECOVERABLE :: ErrorCategory
pattern ErrorCategory_USER_RECOVERABLE = ErrorCategory' "USER_RECOVERABLE"

pattern ErrorCategory_VALIDATION :: ErrorCategory
pattern ErrorCategory_VALIDATION = ErrorCategory' "VALIDATION"

{-# COMPLETE
  ErrorCategory_ACCESS_DENIED,
  ErrorCategory_CANCELLED,
  ErrorCategory_INTERNAL_SERVICE_EXCEPTION,
  ErrorCategory_RESOURCE_NOT_FOUND,
  ErrorCategory_SERVICE_QUOTA_EXCEEDED,
  ErrorCategory_THROTTLING,
  ErrorCategory_USER_RECOVERABLE,
  ErrorCategory_VALIDATION,
  ErrorCategory'
  #-}
