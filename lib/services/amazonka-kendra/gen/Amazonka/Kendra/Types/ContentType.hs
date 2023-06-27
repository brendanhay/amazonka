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
-- Module      : Amazonka.Kendra.Types.ContentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ContentType
  ( ContentType
      ( ..,
        ContentType_CSV,
        ContentType_HTML,
        ContentType_JSON,
        ContentType_MD,
        ContentType_MS_EXCEL,
        ContentType_MS_WORD,
        ContentType_PDF,
        ContentType_PLAIN_TEXT,
        ContentType_PPT,
        ContentType_RTF,
        ContentType_XML,
        ContentType_XSLT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContentType = ContentType'
  { fromContentType ::
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

pattern ContentType_CSV :: ContentType
pattern ContentType_CSV = ContentType' "CSV"

pattern ContentType_HTML :: ContentType
pattern ContentType_HTML = ContentType' "HTML"

pattern ContentType_JSON :: ContentType
pattern ContentType_JSON = ContentType' "JSON"

pattern ContentType_MD :: ContentType
pattern ContentType_MD = ContentType' "MD"

pattern ContentType_MS_EXCEL :: ContentType
pattern ContentType_MS_EXCEL = ContentType' "MS_EXCEL"

pattern ContentType_MS_WORD :: ContentType
pattern ContentType_MS_WORD = ContentType' "MS_WORD"

pattern ContentType_PDF :: ContentType
pattern ContentType_PDF = ContentType' "PDF"

pattern ContentType_PLAIN_TEXT :: ContentType
pattern ContentType_PLAIN_TEXT = ContentType' "PLAIN_TEXT"

pattern ContentType_PPT :: ContentType
pattern ContentType_PPT = ContentType' "PPT"

pattern ContentType_RTF :: ContentType
pattern ContentType_RTF = ContentType' "RTF"

pattern ContentType_XML :: ContentType
pattern ContentType_XML = ContentType' "XML"

pattern ContentType_XSLT :: ContentType
pattern ContentType_XSLT = ContentType' "XSLT"

{-# COMPLETE
  ContentType_CSV,
  ContentType_HTML,
  ContentType_JSON,
  ContentType_MD,
  ContentType_MS_EXCEL,
  ContentType_MS_WORD,
  ContentType_PDF,
  ContentType_PLAIN_TEXT,
  ContentType_PPT,
  ContentType_RTF,
  ContentType_XML,
  ContentType_XSLT,
  ContentType'
  #-}
