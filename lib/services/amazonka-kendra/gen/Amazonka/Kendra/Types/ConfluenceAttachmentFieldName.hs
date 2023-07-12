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
-- Module      : Amazonka.Kendra.Types.ConfluenceAttachmentFieldName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceAttachmentFieldName
  ( ConfluenceAttachmentFieldName
      ( ..,
        ConfluenceAttachmentFieldName_AUTHOR,
        ConfluenceAttachmentFieldName_CONTENT_TYPE,
        ConfluenceAttachmentFieldName_CREATED_DATE,
        ConfluenceAttachmentFieldName_DISPLAY_URL,
        ConfluenceAttachmentFieldName_FILE_SIZE,
        ConfluenceAttachmentFieldName_ITEM_TYPE,
        ConfluenceAttachmentFieldName_PARENT_ID,
        ConfluenceAttachmentFieldName_SPACE_KEY,
        ConfluenceAttachmentFieldName_SPACE_NAME,
        ConfluenceAttachmentFieldName_URL,
        ConfluenceAttachmentFieldName_VERSION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfluenceAttachmentFieldName = ConfluenceAttachmentFieldName'
  { fromConfluenceAttachmentFieldName ::
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

pattern ConfluenceAttachmentFieldName_AUTHOR :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_AUTHOR = ConfluenceAttachmentFieldName' "AUTHOR"

pattern ConfluenceAttachmentFieldName_CONTENT_TYPE :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_CONTENT_TYPE = ConfluenceAttachmentFieldName' "CONTENT_TYPE"

pattern ConfluenceAttachmentFieldName_CREATED_DATE :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_CREATED_DATE = ConfluenceAttachmentFieldName' "CREATED_DATE"

pattern ConfluenceAttachmentFieldName_DISPLAY_URL :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_DISPLAY_URL = ConfluenceAttachmentFieldName' "DISPLAY_URL"

pattern ConfluenceAttachmentFieldName_FILE_SIZE :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_FILE_SIZE = ConfluenceAttachmentFieldName' "FILE_SIZE"

pattern ConfluenceAttachmentFieldName_ITEM_TYPE :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_ITEM_TYPE = ConfluenceAttachmentFieldName' "ITEM_TYPE"

pattern ConfluenceAttachmentFieldName_PARENT_ID :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_PARENT_ID = ConfluenceAttachmentFieldName' "PARENT_ID"

pattern ConfluenceAttachmentFieldName_SPACE_KEY :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_SPACE_KEY = ConfluenceAttachmentFieldName' "SPACE_KEY"

pattern ConfluenceAttachmentFieldName_SPACE_NAME :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_SPACE_NAME = ConfluenceAttachmentFieldName' "SPACE_NAME"

pattern ConfluenceAttachmentFieldName_URL :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_URL = ConfluenceAttachmentFieldName' "URL"

pattern ConfluenceAttachmentFieldName_VERSION :: ConfluenceAttachmentFieldName
pattern ConfluenceAttachmentFieldName_VERSION = ConfluenceAttachmentFieldName' "VERSION"

{-# COMPLETE
  ConfluenceAttachmentFieldName_AUTHOR,
  ConfluenceAttachmentFieldName_CONTENT_TYPE,
  ConfluenceAttachmentFieldName_CREATED_DATE,
  ConfluenceAttachmentFieldName_DISPLAY_URL,
  ConfluenceAttachmentFieldName_FILE_SIZE,
  ConfluenceAttachmentFieldName_ITEM_TYPE,
  ConfluenceAttachmentFieldName_PARENT_ID,
  ConfluenceAttachmentFieldName_SPACE_KEY,
  ConfluenceAttachmentFieldName_SPACE_NAME,
  ConfluenceAttachmentFieldName_URL,
  ConfluenceAttachmentFieldName_VERSION,
  ConfluenceAttachmentFieldName'
  #-}
