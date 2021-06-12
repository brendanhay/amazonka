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
-- Module      : Network.AWS.DMS.Types.EncodingTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EncodingTypeValue
  ( EncodingTypeValue
      ( ..,
        EncodingTypeValue_Plain,
        EncodingTypeValue_Plain_dictionary,
        EncodingTypeValue_Rle_dictionary
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EncodingTypeValue = EncodingTypeValue'
  { fromEncodingTypeValue ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
