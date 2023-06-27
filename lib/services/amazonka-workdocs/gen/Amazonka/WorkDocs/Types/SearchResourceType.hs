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
-- Module      : Amazonka.WorkDocs.Types.SearchResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.SearchResourceType
  ( SearchResourceType
      ( ..,
        SearchResourceType_COMMENT,
        SearchResourceType_DOCUMENT,
        SearchResourceType_DOCUMENT_VERSION,
        SearchResourceType_FOLDER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SearchResourceType = SearchResourceType'
  { fromSearchResourceType ::
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

pattern SearchResourceType_COMMENT :: SearchResourceType
pattern SearchResourceType_COMMENT = SearchResourceType' "COMMENT"

pattern SearchResourceType_DOCUMENT :: SearchResourceType
pattern SearchResourceType_DOCUMENT = SearchResourceType' "DOCUMENT"

pattern SearchResourceType_DOCUMENT_VERSION :: SearchResourceType
pattern SearchResourceType_DOCUMENT_VERSION = SearchResourceType' "DOCUMENT_VERSION"

pattern SearchResourceType_FOLDER :: SearchResourceType
pattern SearchResourceType_FOLDER = SearchResourceType' "FOLDER"

{-# COMPLETE
  SearchResourceType_COMMENT,
  SearchResourceType_DOCUMENT,
  SearchResourceType_DOCUMENT_VERSION,
  SearchResourceType_FOLDER,
  SearchResourceType'
  #-}
