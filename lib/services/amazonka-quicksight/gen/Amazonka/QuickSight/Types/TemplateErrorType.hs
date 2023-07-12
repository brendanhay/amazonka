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
-- Module      : Amazonka.QuickSight.Types.TemplateErrorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateErrorType
  ( TemplateErrorType
      ( ..,
        TemplateErrorType_ACCESS_DENIED,
        TemplateErrorType_DATA_SET_NOT_FOUND,
        TemplateErrorType_INTERNAL_FAILURE,
        TemplateErrorType_SOURCE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TemplateErrorType = TemplateErrorType'
  { fromTemplateErrorType ::
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

pattern TemplateErrorType_ACCESS_DENIED :: TemplateErrorType
pattern TemplateErrorType_ACCESS_DENIED = TemplateErrorType' "ACCESS_DENIED"

pattern TemplateErrorType_DATA_SET_NOT_FOUND :: TemplateErrorType
pattern TemplateErrorType_DATA_SET_NOT_FOUND = TemplateErrorType' "DATA_SET_NOT_FOUND"

pattern TemplateErrorType_INTERNAL_FAILURE :: TemplateErrorType
pattern TemplateErrorType_INTERNAL_FAILURE = TemplateErrorType' "INTERNAL_FAILURE"

pattern TemplateErrorType_SOURCE_NOT_FOUND :: TemplateErrorType
pattern TemplateErrorType_SOURCE_NOT_FOUND = TemplateErrorType' "SOURCE_NOT_FOUND"

{-# COMPLETE
  TemplateErrorType_ACCESS_DENIED,
  TemplateErrorType_DATA_SET_NOT_FOUND,
  TemplateErrorType_INTERNAL_FAILURE,
  TemplateErrorType_SOURCE_NOT_FOUND,
  TemplateErrorType'
  #-}
