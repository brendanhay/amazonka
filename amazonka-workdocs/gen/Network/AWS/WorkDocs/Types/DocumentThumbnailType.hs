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
-- Module      : Network.AWS.WorkDocs.Types.DocumentThumbnailType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentThumbnailType
  ( DocumentThumbnailType
      ( ..,
        DocumentThumbnailType_LARGE,
        DocumentThumbnailType_SMALL,
        DocumentThumbnailType_SMALL_HQ
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DocumentThumbnailType = DocumentThumbnailType'
  { fromDocumentThumbnailType ::
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
