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
-- Module      : Network.AWS.Kendra.Types.ConfluenceBlogFieldName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluenceBlogFieldName
  ( ConfluenceBlogFieldName
      ( ..,
        ConfluenceBlogFieldName_AUTHOR,
        ConfluenceBlogFieldName_DISPLAY_URL,
        ConfluenceBlogFieldName_ITEM_TYPE,
        ConfluenceBlogFieldName_LABELS,
        ConfluenceBlogFieldName_PUBLISH_DATE,
        ConfluenceBlogFieldName_SPACE_KEY,
        ConfluenceBlogFieldName_SPACE_NAME,
        ConfluenceBlogFieldName_URL,
        ConfluenceBlogFieldName_VERSION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConfluenceBlogFieldName = ConfluenceBlogFieldName'
  { fromConfluenceBlogFieldName ::
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

pattern ConfluenceBlogFieldName_AUTHOR :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_AUTHOR = ConfluenceBlogFieldName' "AUTHOR"

pattern ConfluenceBlogFieldName_DISPLAY_URL :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_DISPLAY_URL = ConfluenceBlogFieldName' "DISPLAY_URL"

pattern ConfluenceBlogFieldName_ITEM_TYPE :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_ITEM_TYPE = ConfluenceBlogFieldName' "ITEM_TYPE"

pattern ConfluenceBlogFieldName_LABELS :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_LABELS = ConfluenceBlogFieldName' "LABELS"

pattern ConfluenceBlogFieldName_PUBLISH_DATE :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_PUBLISH_DATE = ConfluenceBlogFieldName' "PUBLISH_DATE"

pattern ConfluenceBlogFieldName_SPACE_KEY :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_SPACE_KEY = ConfluenceBlogFieldName' "SPACE_KEY"

pattern ConfluenceBlogFieldName_SPACE_NAME :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_SPACE_NAME = ConfluenceBlogFieldName' "SPACE_NAME"

pattern ConfluenceBlogFieldName_URL :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_URL = ConfluenceBlogFieldName' "URL"

pattern ConfluenceBlogFieldName_VERSION :: ConfluenceBlogFieldName
pattern ConfluenceBlogFieldName_VERSION = ConfluenceBlogFieldName' "VERSION"

{-# COMPLETE
  ConfluenceBlogFieldName_AUTHOR,
  ConfluenceBlogFieldName_DISPLAY_URL,
  ConfluenceBlogFieldName_ITEM_TYPE,
  ConfluenceBlogFieldName_LABELS,
  ConfluenceBlogFieldName_PUBLISH_DATE,
  ConfluenceBlogFieldName_SPACE_KEY,
  ConfluenceBlogFieldName_SPACE_NAME,
  ConfluenceBlogFieldName_URL,
  ConfluenceBlogFieldName_VERSION,
  ConfluenceBlogFieldName'
  #-}
