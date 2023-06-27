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
-- Module      : Amazonka.Textract.Types.EntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.EntityType
  ( EntityType
      ( ..,
        EntityType_COLUMN_HEADER,
        EntityType_KEY,
        EntityType_SEMI_STRUCTURED_TABLE,
        EntityType_STRUCTURED_TABLE,
        EntityType_TABLE_FOOTER,
        EntityType_TABLE_SECTION_TITLE,
        EntityType_TABLE_SUMMARY,
        EntityType_TABLE_TITLE,
        EntityType_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EntityType = EntityType'
  { fromEntityType ::
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

pattern EntityType_COLUMN_HEADER :: EntityType
pattern EntityType_COLUMN_HEADER = EntityType' "COLUMN_HEADER"

pattern EntityType_KEY :: EntityType
pattern EntityType_KEY = EntityType' "KEY"

pattern EntityType_SEMI_STRUCTURED_TABLE :: EntityType
pattern EntityType_SEMI_STRUCTURED_TABLE = EntityType' "SEMI_STRUCTURED_TABLE"

pattern EntityType_STRUCTURED_TABLE :: EntityType
pattern EntityType_STRUCTURED_TABLE = EntityType' "STRUCTURED_TABLE"

pattern EntityType_TABLE_FOOTER :: EntityType
pattern EntityType_TABLE_FOOTER = EntityType' "TABLE_FOOTER"

pattern EntityType_TABLE_SECTION_TITLE :: EntityType
pattern EntityType_TABLE_SECTION_TITLE = EntityType' "TABLE_SECTION_TITLE"

pattern EntityType_TABLE_SUMMARY :: EntityType
pattern EntityType_TABLE_SUMMARY = EntityType' "TABLE_SUMMARY"

pattern EntityType_TABLE_TITLE :: EntityType
pattern EntityType_TABLE_TITLE = EntityType' "TABLE_TITLE"

pattern EntityType_VALUE :: EntityType
pattern EntityType_VALUE = EntityType' "VALUE"

{-# COMPLETE
  EntityType_COLUMN_HEADER,
  EntityType_KEY,
  EntityType_SEMI_STRUCTURED_TABLE,
  EntityType_STRUCTURED_TABLE,
  EntityType_TABLE_FOOTER,
  EntityType_TABLE_SECTION_TITLE,
  EntityType_TABLE_SUMMARY,
  EntityType_TABLE_TITLE,
  EntityType_VALUE,
  EntityType'
  #-}
