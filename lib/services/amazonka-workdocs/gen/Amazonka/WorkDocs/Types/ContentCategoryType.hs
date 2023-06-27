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
-- Module      : Amazonka.WorkDocs.Types.ContentCategoryType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ContentCategoryType
  ( ContentCategoryType
      ( ..,
        ContentCategoryType_AUDIO,
        ContentCategoryType_DOCUMENT,
        ContentCategoryType_IMAGE,
        ContentCategoryType_OTHER,
        ContentCategoryType_PDF,
        ContentCategoryType_PRESENTATION,
        ContentCategoryType_SOURCE_CODE,
        ContentCategoryType_SPREADSHEET,
        ContentCategoryType_VIDEO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContentCategoryType = ContentCategoryType'
  { fromContentCategoryType ::
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

pattern ContentCategoryType_AUDIO :: ContentCategoryType
pattern ContentCategoryType_AUDIO = ContentCategoryType' "AUDIO"

pattern ContentCategoryType_DOCUMENT :: ContentCategoryType
pattern ContentCategoryType_DOCUMENT = ContentCategoryType' "DOCUMENT"

pattern ContentCategoryType_IMAGE :: ContentCategoryType
pattern ContentCategoryType_IMAGE = ContentCategoryType' "IMAGE"

pattern ContentCategoryType_OTHER :: ContentCategoryType
pattern ContentCategoryType_OTHER = ContentCategoryType' "OTHER"

pattern ContentCategoryType_PDF :: ContentCategoryType
pattern ContentCategoryType_PDF = ContentCategoryType' "PDF"

pattern ContentCategoryType_PRESENTATION :: ContentCategoryType
pattern ContentCategoryType_PRESENTATION = ContentCategoryType' "PRESENTATION"

pattern ContentCategoryType_SOURCE_CODE :: ContentCategoryType
pattern ContentCategoryType_SOURCE_CODE = ContentCategoryType' "SOURCE_CODE"

pattern ContentCategoryType_SPREADSHEET :: ContentCategoryType
pattern ContentCategoryType_SPREADSHEET = ContentCategoryType' "SPREADSHEET"

pattern ContentCategoryType_VIDEO :: ContentCategoryType
pattern ContentCategoryType_VIDEO = ContentCategoryType' "VIDEO"

{-# COMPLETE
  ContentCategoryType_AUDIO,
  ContentCategoryType_DOCUMENT,
  ContentCategoryType_IMAGE,
  ContentCategoryType_OTHER,
  ContentCategoryType_PDF,
  ContentCategoryType_PRESENTATION,
  ContentCategoryType_SOURCE_CODE,
  ContentCategoryType_SPREADSHEET,
  ContentCategoryType_VIDEO,
  ContentCategoryType'
  #-}
