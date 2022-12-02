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
-- Module      : Amazonka.DMS.Types.EncodingTypeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.EncodingTypeValue
  ( EncodingTypeValue
      ( ..,
        EncodingTypeValue_Plain,
        EncodingTypeValue_Plain_dictionary,
        EncodingTypeValue_Rle_dictionary
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncodingTypeValue = EncodingTypeValue'
  { fromEncodingTypeValue ::
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

pattern EncodingTypeValue_Plain :: EncodingTypeValue
pattern EncodingTypeValue_Plain = EncodingTypeValue' "plain"

pattern EncodingTypeValue_Plain_dictionary :: EncodingTypeValue
pattern EncodingTypeValue_Plain_dictionary = EncodingTypeValue' "plain-dictionary"

pattern EncodingTypeValue_Rle_dictionary :: EncodingTypeValue
pattern EncodingTypeValue_Rle_dictionary = EncodingTypeValue' "rle-dictionary"

{-# COMPLETE
  EncodingTypeValue_Plain,
  EncodingTypeValue_Plain_dictionary,
  EncodingTypeValue_Rle_dictionary,
  EncodingTypeValue'
  #-}
