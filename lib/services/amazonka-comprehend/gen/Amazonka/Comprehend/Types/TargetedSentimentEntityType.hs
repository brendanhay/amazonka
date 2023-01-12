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
-- Module      : Amazonka.Comprehend.Types.TargetedSentimentEntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TargetedSentimentEntityType
  ( TargetedSentimentEntityType
      ( ..,
        TargetedSentimentEntityType_ATTRIBUTE,
        TargetedSentimentEntityType_BOOK,
        TargetedSentimentEntityType_BRAND,
        TargetedSentimentEntityType_COMMERCIAL_ITEM,
        TargetedSentimentEntityType_DATE,
        TargetedSentimentEntityType_EVENT,
        TargetedSentimentEntityType_FACILITY,
        TargetedSentimentEntityType_GAME,
        TargetedSentimentEntityType_LOCATION,
        TargetedSentimentEntityType_MOVIE,
        TargetedSentimentEntityType_MUSIC,
        TargetedSentimentEntityType_ORGANIZATION,
        TargetedSentimentEntityType_OTHER,
        TargetedSentimentEntityType_PERSON,
        TargetedSentimentEntityType_PERSONAL_TITLE,
        TargetedSentimentEntityType_QUANTITY,
        TargetedSentimentEntityType_SOFTWARE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetedSentimentEntityType = TargetedSentimentEntityType'
  { fromTargetedSentimentEntityType ::
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

pattern TargetedSentimentEntityType_ATTRIBUTE :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_ATTRIBUTE = TargetedSentimentEntityType' "ATTRIBUTE"

pattern TargetedSentimentEntityType_BOOK :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_BOOK = TargetedSentimentEntityType' "BOOK"

pattern TargetedSentimentEntityType_BRAND :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_BRAND = TargetedSentimentEntityType' "BRAND"

pattern TargetedSentimentEntityType_COMMERCIAL_ITEM :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_COMMERCIAL_ITEM = TargetedSentimentEntityType' "COMMERCIAL_ITEM"

pattern TargetedSentimentEntityType_DATE :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_DATE = TargetedSentimentEntityType' "DATE"

pattern TargetedSentimentEntityType_EVENT :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_EVENT = TargetedSentimentEntityType' "EVENT"

pattern TargetedSentimentEntityType_FACILITY :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_FACILITY = TargetedSentimentEntityType' "FACILITY"

pattern TargetedSentimentEntityType_GAME :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_GAME = TargetedSentimentEntityType' "GAME"

pattern TargetedSentimentEntityType_LOCATION :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_LOCATION = TargetedSentimentEntityType' "LOCATION"

pattern TargetedSentimentEntityType_MOVIE :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_MOVIE = TargetedSentimentEntityType' "MOVIE"

pattern TargetedSentimentEntityType_MUSIC :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_MUSIC = TargetedSentimentEntityType' "MUSIC"

pattern TargetedSentimentEntityType_ORGANIZATION :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_ORGANIZATION = TargetedSentimentEntityType' "ORGANIZATION"

pattern TargetedSentimentEntityType_OTHER :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_OTHER = TargetedSentimentEntityType' "OTHER"

pattern TargetedSentimentEntityType_PERSON :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_PERSON = TargetedSentimentEntityType' "PERSON"

pattern TargetedSentimentEntityType_PERSONAL_TITLE :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_PERSONAL_TITLE = TargetedSentimentEntityType' "PERSONAL_TITLE"

pattern TargetedSentimentEntityType_QUANTITY :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_QUANTITY = TargetedSentimentEntityType' "QUANTITY"

pattern TargetedSentimentEntityType_SOFTWARE :: TargetedSentimentEntityType
pattern TargetedSentimentEntityType_SOFTWARE = TargetedSentimentEntityType' "SOFTWARE"

{-# COMPLETE
  TargetedSentimentEntityType_ATTRIBUTE,
  TargetedSentimentEntityType_BOOK,
  TargetedSentimentEntityType_BRAND,
  TargetedSentimentEntityType_COMMERCIAL_ITEM,
  TargetedSentimentEntityType_DATE,
  TargetedSentimentEntityType_EVENT,
  TargetedSentimentEntityType_FACILITY,
  TargetedSentimentEntityType_GAME,
  TargetedSentimentEntityType_LOCATION,
  TargetedSentimentEntityType_MOVIE,
  TargetedSentimentEntityType_MUSIC,
  TargetedSentimentEntityType_ORGANIZATION,
  TargetedSentimentEntityType_OTHER,
  TargetedSentimentEntityType_PERSON,
  TargetedSentimentEntityType_PERSONAL_TITLE,
  TargetedSentimentEntityType_QUANTITY,
  TargetedSentimentEntityType_SOFTWARE,
  TargetedSentimentEntityType'
  #-}
