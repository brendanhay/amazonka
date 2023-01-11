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
-- Module      : Amazonka.Kendra.Types.ConfluenceSpaceFieldName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceSpaceFieldName
  ( ConfluenceSpaceFieldName
      ( ..,
        ConfluenceSpaceFieldName_DISPLAY_URL,
        ConfluenceSpaceFieldName_ITEM_TYPE,
        ConfluenceSpaceFieldName_SPACE_KEY,
        ConfluenceSpaceFieldName_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfluenceSpaceFieldName = ConfluenceSpaceFieldName'
  { fromConfluenceSpaceFieldName ::
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

pattern ConfluenceSpaceFieldName_DISPLAY_URL :: ConfluenceSpaceFieldName
pattern ConfluenceSpaceFieldName_DISPLAY_URL = ConfluenceSpaceFieldName' "DISPLAY_URL"

pattern ConfluenceSpaceFieldName_ITEM_TYPE :: ConfluenceSpaceFieldName
pattern ConfluenceSpaceFieldName_ITEM_TYPE = ConfluenceSpaceFieldName' "ITEM_TYPE"

pattern ConfluenceSpaceFieldName_SPACE_KEY :: ConfluenceSpaceFieldName
pattern ConfluenceSpaceFieldName_SPACE_KEY = ConfluenceSpaceFieldName' "SPACE_KEY"

pattern ConfluenceSpaceFieldName_URL :: ConfluenceSpaceFieldName
pattern ConfluenceSpaceFieldName_URL = ConfluenceSpaceFieldName' "URL"

{-# COMPLETE
  ConfluenceSpaceFieldName_DISPLAY_URL,
  ConfluenceSpaceFieldName_ITEM_TYPE,
  ConfluenceSpaceFieldName_SPACE_KEY,
  ConfluenceSpaceFieldName_URL,
  ConfluenceSpaceFieldName'
  #-}
