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
-- Module      : Amazonka.Comprehend.Types.DocumentReadFeatureTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentReadFeatureTypes
  ( DocumentReadFeatureTypes
      ( ..,
        DocumentReadFeatureTypes_FORMS,
        DocumentReadFeatureTypes_TABLES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the type of Amazon Textract features to apply. If you chose
-- @TEXTRACT_ANALYZE_DOCUMENT@ as the read action, you must specify one or
-- both of the following values:
--
-- -   @TABLES@ - Returns additional information about any tables that are
--     detected in the input document.
--
-- -   @FORMS@ - Returns additional information about any forms that are
--     detected in the input document.
newtype DocumentReadFeatureTypes = DocumentReadFeatureTypes'
  { fromDocumentReadFeatureTypes ::
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

pattern DocumentReadFeatureTypes_FORMS :: DocumentReadFeatureTypes
pattern DocumentReadFeatureTypes_FORMS = DocumentReadFeatureTypes' "FORMS"

pattern DocumentReadFeatureTypes_TABLES :: DocumentReadFeatureTypes
pattern DocumentReadFeatureTypes_TABLES = DocumentReadFeatureTypes' "TABLES"

{-# COMPLETE
  DocumentReadFeatureTypes_FORMS,
  DocumentReadFeatureTypes_TABLES,
  DocumentReadFeatureTypes'
  #-}
