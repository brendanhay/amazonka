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
-- Module      : Amazonka.CloudSearch.Types.SuggesterFuzzyMatching
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.SuggesterFuzzyMatching
  ( SuggesterFuzzyMatching
      ( ..,
        SuggesterFuzzyMatching_High,
        SuggesterFuzzyMatching_Low,
        SuggesterFuzzyMatching_None
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SuggesterFuzzyMatching = SuggesterFuzzyMatching'
  { fromSuggesterFuzzyMatching ::
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

pattern SuggesterFuzzyMatching_High :: SuggesterFuzzyMatching
pattern SuggesterFuzzyMatching_High = SuggesterFuzzyMatching' "high"

pattern SuggesterFuzzyMatching_Low :: SuggesterFuzzyMatching
pattern SuggesterFuzzyMatching_Low = SuggesterFuzzyMatching' "low"

pattern SuggesterFuzzyMatching_None :: SuggesterFuzzyMatching
pattern SuggesterFuzzyMatching_None = SuggesterFuzzyMatching' "none"

{-# COMPLETE
  SuggesterFuzzyMatching_High,
  SuggesterFuzzyMatching_Low,
  SuggesterFuzzyMatching_None,
  SuggesterFuzzyMatching'
  #-}
