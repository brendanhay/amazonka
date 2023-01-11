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
-- Module      : Amazonka.CloudSearch.Types.IndexFieldType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.IndexFieldType
  ( IndexFieldType
      ( ..,
        IndexFieldType_Date,
        IndexFieldType_Date_array,
        IndexFieldType_Double,
        IndexFieldType_Double_array,
        IndexFieldType_Int,
        IndexFieldType_Int_array,
        IndexFieldType_Latlon,
        IndexFieldType_Literal,
        IndexFieldType_Literal_array,
        IndexFieldType_Text,
        IndexFieldType_Text_array
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of field. The valid options for a field depend on the field
-- type. For more information about the supported field types, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
newtype IndexFieldType = IndexFieldType'
  { fromIndexFieldType ::
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

pattern IndexFieldType_Date :: IndexFieldType
pattern IndexFieldType_Date = IndexFieldType' "date"

pattern IndexFieldType_Date_array :: IndexFieldType
pattern IndexFieldType_Date_array = IndexFieldType' "date-array"

pattern IndexFieldType_Double :: IndexFieldType
pattern IndexFieldType_Double = IndexFieldType' "double"

pattern IndexFieldType_Double_array :: IndexFieldType
pattern IndexFieldType_Double_array = IndexFieldType' "double-array"

pattern IndexFieldType_Int :: IndexFieldType
pattern IndexFieldType_Int = IndexFieldType' "int"

pattern IndexFieldType_Int_array :: IndexFieldType
pattern IndexFieldType_Int_array = IndexFieldType' "int-array"

pattern IndexFieldType_Latlon :: IndexFieldType
pattern IndexFieldType_Latlon = IndexFieldType' "latlon"

pattern IndexFieldType_Literal :: IndexFieldType
pattern IndexFieldType_Literal = IndexFieldType' "literal"

pattern IndexFieldType_Literal_array :: IndexFieldType
pattern IndexFieldType_Literal_array = IndexFieldType' "literal-array"

pattern IndexFieldType_Text :: IndexFieldType
pattern IndexFieldType_Text = IndexFieldType' "text"

pattern IndexFieldType_Text_array :: IndexFieldType
pattern IndexFieldType_Text_array = IndexFieldType' "text-array"

{-# COMPLETE
  IndexFieldType_Date,
  IndexFieldType_Date_array,
  IndexFieldType_Double,
  IndexFieldType_Double_array,
  IndexFieldType_Int,
  IndexFieldType_Int_array,
  IndexFieldType_Latlon,
  IndexFieldType_Literal,
  IndexFieldType_Literal_array,
  IndexFieldType_Text,
  IndexFieldType_Text_array,
  IndexFieldType'
  #-}
