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
-- Module      : Amazonka.Comprehend.Types.PageBasedWarningCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PageBasedWarningCode
  ( PageBasedWarningCode
      ( ..,
        PageBasedWarningCode_INFERENCING_NATIVE_DOCUMENT_WITH_PLAINTEXT_TRAINED_MODEL,
        PageBasedWarningCode_INFERENCING_PLAINTEXT_WITH_NATIVE_TRAINED_MODEL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PageBasedWarningCode = PageBasedWarningCode'
  { fromPageBasedWarningCode ::
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

pattern PageBasedWarningCode_INFERENCING_NATIVE_DOCUMENT_WITH_PLAINTEXT_TRAINED_MODEL :: PageBasedWarningCode
pattern PageBasedWarningCode_INFERENCING_NATIVE_DOCUMENT_WITH_PLAINTEXT_TRAINED_MODEL = PageBasedWarningCode' "INFERENCING_NATIVE_DOCUMENT_WITH_PLAINTEXT_TRAINED_MODEL"

pattern PageBasedWarningCode_INFERENCING_PLAINTEXT_WITH_NATIVE_TRAINED_MODEL :: PageBasedWarningCode
pattern PageBasedWarningCode_INFERENCING_PLAINTEXT_WITH_NATIVE_TRAINED_MODEL = PageBasedWarningCode' "INFERENCING_PLAINTEXT_WITH_NATIVE_TRAINED_MODEL"

{-# COMPLETE
  PageBasedWarningCode_INFERENCING_NATIVE_DOCUMENT_WITH_PLAINTEXT_TRAINED_MODEL,
  PageBasedWarningCode_INFERENCING_PLAINTEXT_WITH_NATIVE_TRAINED_MODEL,
  PageBasedWarningCode'
  #-}
