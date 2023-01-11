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
-- Module      : Amazonka.CloudSearch.Types.AlgorithmicStemming
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.AlgorithmicStemming
  ( AlgorithmicStemming
      ( ..,
        AlgorithmicStemming_Full,
        AlgorithmicStemming_Light,
        AlgorithmicStemming_Minimal,
        AlgorithmicStemming_None
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlgorithmicStemming = AlgorithmicStemming'
  { fromAlgorithmicStemming ::
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
