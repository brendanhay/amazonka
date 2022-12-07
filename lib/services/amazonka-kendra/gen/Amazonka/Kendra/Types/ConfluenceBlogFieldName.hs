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
-- Module      : Amazonka.Kendra.Types.ConfluenceBlogFieldName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceBlogFieldName
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfluenceBlogFieldName = ConfluenceBlogFieldName'
  { fromConfluenceBlogFieldName ::
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
