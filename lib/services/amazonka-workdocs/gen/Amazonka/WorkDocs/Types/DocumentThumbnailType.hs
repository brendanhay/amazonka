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
-- Module      : Amazonka.WorkDocs.Types.DocumentThumbnailType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.DocumentThumbnailType
  ( DocumentThumbnailType
      ( ..,
        DocumentThumbnailType_LARGE,
        DocumentThumbnailType_SMALL,
        DocumentThumbnailType_SMALL_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentThumbnailType = DocumentThumbnailType'
  { fromDocumentThumbnailType ::
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

pattern DocumentThumbnailType_LARGE :: DocumentThumbnailType
pattern DocumentThumbnailType_LARGE = DocumentThumbnailType' "LARGE"

pattern DocumentThumbnailType_SMALL :: DocumentThumbnailType
pattern DocumentThumbnailType_SMALL = DocumentThumbnailType' "SMALL"

pattern DocumentThumbnailType_SMALL_HQ :: DocumentThumbnailType
pattern DocumentThumbnailType_SMALL_HQ = DocumentThumbnailType' "SMALL_HQ"

{-# COMPLETE
  DocumentThumbnailType_LARGE,
  DocumentThumbnailType_SMALL,
  DocumentThumbnailType_SMALL_HQ,
  DocumentThumbnailType'
  #-}
