{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AlgorithmicStemming
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AlgorithmicStemming
  ( AlgorithmicStemming
      ( ..,
        AlgorithmicStemming_Full,
        AlgorithmicStemming_Light,
        AlgorithmicStemming_Minimal,
        AlgorithmicStemming_None
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AlgorithmicStemming = AlgorithmicStemming'
  { fromAlgorithmicStemming ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AlgorithmicStemming_Full :: AlgorithmicStemming
pattern AlgorithmicStemming_Full = AlgorithmicStemming' "full"

pattern AlgorithmicStemming_Light :: AlgorithmicStemming
pattern AlgorithmicStemming_Light = AlgorithmicStemming' "light"

pattern AlgorithmicStemming_Minimal :: AlgorithmicStemming
pattern AlgorithmicStemming_Minimal = AlgorithmicStemming' "minimal"

pattern AlgorithmicStemming_None :: AlgorithmicStemming
pattern AlgorithmicStemming_None = AlgorithmicStemming' "none"

{-# COMPLETE
  AlgorithmicStemming_Full,
  AlgorithmicStemming_Light,
  AlgorithmicStemming_Minimal,
  AlgorithmicStemming_None,
  AlgorithmicStemming'
  #-}
