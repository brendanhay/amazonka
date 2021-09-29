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
-- Module      : Network.AWS.Comprehend.Types.DocumentReadFeatureTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentReadFeatureTypes
  ( DocumentReadFeatureTypes
      ( ..,
        DocumentReadFeatureTypes_FORMS,
        DocumentReadFeatureTypes_TABLES
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | A list of the types of analyses to perform. This field specifies what
-- feature types need to be extracted from the document where entity
-- recognition is expected.
--
-- -   @TABLES@ - Add TABLES to the list to return information about the
--     tables that are detected in the input document.
--
-- -   @FORMS@ - Add FORMS to return detected form data.
newtype DocumentReadFeatureTypes = DocumentReadFeatureTypes'
  { fromDocumentReadFeatureTypes ::
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

pattern DocumentReadFeatureTypes_FORMS :: DocumentReadFeatureTypes
pattern DocumentReadFeatureTypes_FORMS = DocumentReadFeatureTypes' "FORMS"

pattern DocumentReadFeatureTypes_TABLES :: DocumentReadFeatureTypes
pattern DocumentReadFeatureTypes_TABLES = DocumentReadFeatureTypes' "TABLES"

{-# COMPLETE
  DocumentReadFeatureTypes_FORMS,
  DocumentReadFeatureTypes_TABLES,
  DocumentReadFeatureTypes'
  #-}
