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
-- Module      : Amazonka.Comprehend.Types.DocumentClassifierDataFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassifierDataFormat
  ( DocumentClassifierDataFormat
      ( ..,
        DocumentClassifierDataFormat_AUGMENTED_MANIFEST,
        DocumentClassifierDataFormat_COMPREHEND_CSV
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentClassifierDataFormat = DocumentClassifierDataFormat'
  { fromDocumentClassifierDataFormat ::
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

pattern DocumentClassifierDataFormat_AUGMENTED_MANIFEST :: DocumentClassifierDataFormat
pattern DocumentClassifierDataFormat_AUGMENTED_MANIFEST = DocumentClassifierDataFormat' "AUGMENTED_MANIFEST"

pattern DocumentClassifierDataFormat_COMPREHEND_CSV :: DocumentClassifierDataFormat
pattern DocumentClassifierDataFormat_COMPREHEND_CSV = DocumentClassifierDataFormat' "COMPREHEND_CSV"

{-# COMPLETE
  DocumentClassifierDataFormat_AUGMENTED_MANIFEST,
  DocumentClassifierDataFormat_COMPREHEND_CSV,
  DocumentClassifierDataFormat'
  #-}
