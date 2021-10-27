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
-- Module      : Network.AWS.Kendra.Types.ContentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ContentType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ContentType = ContentType'
  { fromContentType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
