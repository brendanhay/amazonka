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
-- Module      : Network.AWS.Kendra.Types.ConfluencePageFieldName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluencePageFieldName
  ( ConfluencePageFieldName
      ( ..,
        ConfluencePageFieldName_AUTHOR,
        ConfluencePageFieldName_CONTENT_STATUS,
        ConfluencePageFieldName_CREATED_DATE,
        ConfluencePageFieldName_DISPLAY_URL,
        ConfluencePageFieldName_ITEM_TYPE,
        ConfluencePageFieldName_LABELS,
        ConfluencePageFieldName_MODIFIED_DATE,
        ConfluencePageFieldName_PARENT_ID,
        ConfluencePageFieldName_SPACE_KEY,
        ConfluencePageFieldName_SPACE_NAME,
        ConfluencePageFieldName_URL,
        ConfluencePageFieldName_VERSION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ConfluencePageFieldName = ConfluencePageFieldName'
  { fromConfluencePageFieldName ::
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

pattern ConfluencePageFieldName_AUTHOR :: ConfluencePageFieldName
pattern ConfluencePageFieldName_AUTHOR = ConfluencePageFieldName' "AUTHOR"

pattern ConfluencePageFieldName_CONTENT_STATUS :: ConfluencePageFieldName
pattern ConfluencePageFieldName_CONTENT_STATUS = ConfluencePageFieldName' "CONTENT_STATUS"

pattern ConfluencePageFieldName_CREATED_DATE :: ConfluencePageFieldName
pattern ConfluencePageFieldName_CREATED_DATE = ConfluencePageFieldName' "CREATED_DATE"

pattern ConfluencePageFieldName_DISPLAY_URL :: ConfluencePageFieldName
pattern ConfluencePageFieldName_DISPLAY_URL = ConfluencePageFieldName' "DISPLAY_URL"

pattern ConfluencePageFieldName_ITEM_TYPE :: ConfluencePageFieldName
pattern ConfluencePageFieldName_ITEM_TYPE = ConfluencePageFieldName' "ITEM_TYPE"

pattern ConfluencePageFieldName_LABELS :: ConfluencePageFieldName
pattern ConfluencePageFieldName_LABELS = ConfluencePageFieldName' "LABELS"

pattern ConfluencePageFieldName_MODIFIED_DATE :: ConfluencePageFieldName
pattern ConfluencePageFieldName_MODIFIED_DATE = ConfluencePageFieldName' "MODIFIED_DATE"

pattern ConfluencePageFieldName_PARENT_ID :: ConfluencePageFieldName
pattern ConfluencePageFieldName_PARENT_ID = ConfluencePageFieldName' "PARENT_ID"

pattern ConfluencePageFieldName_SPACE_KEY :: ConfluencePageFieldName
pattern ConfluencePageFieldName_SPACE_KEY = ConfluencePageFieldName' "SPACE_KEY"

pattern ConfluencePageFieldName_SPACE_NAME :: ConfluencePageFieldName
pattern ConfluencePageFieldName_SPACE_NAME = ConfluencePageFieldName' "SPACE_NAME"

pattern ConfluencePageFieldName_URL :: ConfluencePageFieldName
pattern ConfluencePageFieldName_URL = ConfluencePageFieldName' "URL"

pattern ConfluencePageFieldName_VERSION :: ConfluencePageFieldName
pattern ConfluencePageFieldName_VERSION = ConfluencePageFieldName' "VERSION"

{-# COMPLETE
  ConfluencePageFieldName_AUTHOR,
  ConfluencePageFieldName_CONTENT_STATUS,
  ConfluencePageFieldName_CREATED_DATE,
  ConfluencePageFieldName_DISPLAY_URL,
  ConfluencePageFieldName_ITEM_TYPE,
  ConfluencePageFieldName_LABELS,
  ConfluencePageFieldName_MODIFIED_DATE,
  ConfluencePageFieldName_PARENT_ID,
  ConfluencePageFieldName_SPACE_KEY,
  ConfluencePageFieldName_SPACE_NAME,
  ConfluencePageFieldName_URL,
  ConfluencePageFieldName_VERSION,
  ConfluencePageFieldName'
  #-}
