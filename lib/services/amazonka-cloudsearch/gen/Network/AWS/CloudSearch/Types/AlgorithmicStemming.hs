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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype AlgorithmicStemming = AlgorithmicStemming'
  { fromAlgorithmicStemming ::
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
