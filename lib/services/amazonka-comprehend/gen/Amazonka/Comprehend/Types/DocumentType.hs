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
-- Module      : Amazonka.Comprehend.Types.DocumentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentType
  ( DocumentType
      ( ..,
        DocumentType_IMAGE,
        DocumentType_MS_WORD,
        DocumentType_NATIVE_PDF,
        DocumentType_PLAIN_TEXT,
        DocumentType_SCANNED_PDF,
        DocumentType_TEXTRACT_ANALYZE_DOCUMENT_JSON,
        DocumentType_TEXTRACT_DETECT_DOCUMENT_TEXT_JSON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentType = DocumentType'
  { fromDocumentType ::
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

pattern DocumentType_IMAGE :: DocumentType
pattern DocumentType_IMAGE = DocumentType' "IMAGE"

pattern DocumentType_MS_WORD :: DocumentType
pattern DocumentType_MS_WORD = DocumentType' "MS_WORD"

pattern DocumentType_NATIVE_PDF :: DocumentType
pattern DocumentType_NATIVE_PDF = DocumentType' "NATIVE_PDF"

pattern DocumentType_PLAIN_TEXT :: DocumentType
pattern DocumentType_PLAIN_TEXT = DocumentType' "PLAIN_TEXT"

pattern DocumentType_SCANNED_PDF :: DocumentType
pattern DocumentType_SCANNED_PDF = DocumentType' "SCANNED_PDF"

pattern DocumentType_TEXTRACT_ANALYZE_DOCUMENT_JSON :: DocumentType
pattern DocumentType_TEXTRACT_ANALYZE_DOCUMENT_JSON = DocumentType' "TEXTRACT_ANALYZE_DOCUMENT_JSON"

pattern DocumentType_TEXTRACT_DETECT_DOCUMENT_TEXT_JSON :: DocumentType
pattern DocumentType_TEXTRACT_DETECT_DOCUMENT_TEXT_JSON = DocumentType' "TEXTRACT_DETECT_DOCUMENT_TEXT_JSON"

{-# COMPLETE
  DocumentType_IMAGE,
  DocumentType_MS_WORD,
  DocumentType_NATIVE_PDF,
  DocumentType_PLAIN_TEXT,
  DocumentType_SCANNED_PDF,
  DocumentType_TEXTRACT_ANALYZE_DOCUMENT_JSON,
  DocumentType_TEXTRACT_DETECT_DOCUMENT_TEXT_JSON,
  DocumentType'
  #-}
