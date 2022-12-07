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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ContentType
  ( ContentType
      ( ..,
        ContentType_HTML,
        ContentType_MS_WORD,
        ContentType_PDF,
        ContentType_PLAIN_TEXT,
        ContentType_PPT
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

pattern ContentType_HTML :: ContentType
pattern ContentType_HTML = ContentType' "HTML"

pattern ContentType_MS_WORD :: ContentType
pattern ContentType_MS_WORD = ContentType' "MS_WORD"

pattern ContentType_PDF :: ContentType
pattern ContentType_PDF = ContentType' "PDF"

pattern ContentType_PLAIN_TEXT :: ContentType
pattern ContentType_PLAIN_TEXT = ContentType' "PLAIN_TEXT"

pattern ContentType_PPT :: ContentType
pattern ContentType_PPT = ContentType' "PPT"

{-# COMPLETE
  ContentType_HTML,
  ContentType_MS_WORD,
  ContentType_PDF,
  ContentType_PLAIN_TEXT,
  ContentType_PPT,
  ContentType'
  #-}
