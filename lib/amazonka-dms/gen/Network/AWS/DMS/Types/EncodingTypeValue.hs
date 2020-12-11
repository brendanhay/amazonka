-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.EncodingTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.EncodingTypeValue
  ( EncodingTypeValue
      ( EncodingTypeValue',
        Plain,
        PlainDictionary,
        RleDictionary
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EncodingTypeValue = EncodingTypeValue' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Plain :: EncodingTypeValue
pattern Plain = EncodingTypeValue' "plain"

pattern PlainDictionary :: EncodingTypeValue
pattern PlainDictionary = EncodingTypeValue' "plain-dictionary"

pattern RleDictionary :: EncodingTypeValue
pattern RleDictionary = EncodingTypeValue' "rle-dictionary"

{-# COMPLETE
  Plain,
  PlainDictionary,
  RleDictionary,
  EncodingTypeValue'
  #-}
