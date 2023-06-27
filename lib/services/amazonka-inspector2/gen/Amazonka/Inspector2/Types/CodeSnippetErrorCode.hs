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
-- Module      : Amazonka.Inspector2.Types.CodeSnippetErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CodeSnippetErrorCode
  ( CodeSnippetErrorCode
      ( ..,
        CodeSnippetErrorCode_ACCESS_DENIED,
        CodeSnippetErrorCode_CODE_SNIPPET_NOT_FOUND,
        CodeSnippetErrorCode_INTERNAL_ERROR,
        CodeSnippetErrorCode_INVALID_INPUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CodeSnippetErrorCode = CodeSnippetErrorCode'
  { fromCodeSnippetErrorCode ::
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

pattern CodeSnippetErrorCode_ACCESS_DENIED :: CodeSnippetErrorCode
pattern CodeSnippetErrorCode_ACCESS_DENIED = CodeSnippetErrorCode' "ACCESS_DENIED"

pattern CodeSnippetErrorCode_CODE_SNIPPET_NOT_FOUND :: CodeSnippetErrorCode
pattern CodeSnippetErrorCode_CODE_SNIPPET_NOT_FOUND = CodeSnippetErrorCode' "CODE_SNIPPET_NOT_FOUND"

pattern CodeSnippetErrorCode_INTERNAL_ERROR :: CodeSnippetErrorCode
pattern CodeSnippetErrorCode_INTERNAL_ERROR = CodeSnippetErrorCode' "INTERNAL_ERROR"

pattern CodeSnippetErrorCode_INVALID_INPUT :: CodeSnippetErrorCode
pattern CodeSnippetErrorCode_INVALID_INPUT = CodeSnippetErrorCode' "INVALID_INPUT"

{-# COMPLETE
  CodeSnippetErrorCode_ACCESS_DENIED,
  CodeSnippetErrorCode_CODE_SNIPPET_NOT_FOUND,
  CodeSnippetErrorCode_INTERNAL_ERROR,
  CodeSnippetErrorCode_INVALID_INPUT,
  CodeSnippetErrorCode'
  #-}
