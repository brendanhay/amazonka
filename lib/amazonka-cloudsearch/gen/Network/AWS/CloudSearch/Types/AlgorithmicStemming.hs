{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AlgorithmicStemming
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AlgorithmicStemming
  ( AlgorithmicStemming
      ( AlgorithmicStemming',
        ASNone,
        ASMinimal,
        ASLight,
        ASFull
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AlgorithmicStemming = AlgorithmicStemming' Lude.Text
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

pattern ASNone :: AlgorithmicStemming
pattern ASNone = AlgorithmicStemming' "none"

pattern ASMinimal :: AlgorithmicStemming
pattern ASMinimal = AlgorithmicStemming' "minimal"

pattern ASLight :: AlgorithmicStemming
pattern ASLight = AlgorithmicStemming' "light"

pattern ASFull :: AlgorithmicStemming
pattern ASFull = AlgorithmicStemming' "full"

{-# COMPLETE
  ASNone,
  ASMinimal,
  ASLight,
  ASFull,
  AlgorithmicStemming'
  #-}
