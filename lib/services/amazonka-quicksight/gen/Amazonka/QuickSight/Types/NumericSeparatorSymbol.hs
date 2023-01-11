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
-- Module      : Amazonka.QuickSight.Types.NumericSeparatorSymbol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericSeparatorSymbol
  ( NumericSeparatorSymbol
      ( ..,
        NumericSeparatorSymbol_COMMA,
        NumericSeparatorSymbol_DOT,
        NumericSeparatorSymbol_SPACE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NumericSeparatorSymbol = NumericSeparatorSymbol'
  { fromNumericSeparatorSymbol ::
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

pattern NumericSeparatorSymbol_COMMA :: NumericSeparatorSymbol
pattern NumericSeparatorSymbol_COMMA = NumericSeparatorSymbol' "COMMA"

pattern NumericSeparatorSymbol_DOT :: NumericSeparatorSymbol
pattern NumericSeparatorSymbol_DOT = NumericSeparatorSymbol' "DOT"

pattern NumericSeparatorSymbol_SPACE :: NumericSeparatorSymbol
pattern NumericSeparatorSymbol_SPACE = NumericSeparatorSymbol' "SPACE"

{-# COMPLETE
  NumericSeparatorSymbol_COMMA,
  NumericSeparatorSymbol_DOT,
  NumericSeparatorSymbol_SPACE,
  NumericSeparatorSymbol'
  #-}
