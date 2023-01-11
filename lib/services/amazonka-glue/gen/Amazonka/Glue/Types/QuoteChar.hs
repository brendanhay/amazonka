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
-- Module      : Amazonka.Glue.Types.QuoteChar
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.QuoteChar
  ( QuoteChar
      ( ..,
        QuoteChar_Disabled,
        QuoteChar_Quillemet,
        QuoteChar_Quote,
        QuoteChar_Single_quote
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QuoteChar = QuoteChar'
  { fromQuoteChar ::
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

pattern QuoteChar_Disabled :: QuoteChar
pattern QuoteChar_Disabled = QuoteChar' "disabled"

pattern QuoteChar_Quillemet :: QuoteChar
pattern QuoteChar_Quillemet = QuoteChar' "quillemet"

pattern QuoteChar_Quote :: QuoteChar
pattern QuoteChar_Quote = QuoteChar' "quote"

pattern QuoteChar_Single_quote :: QuoteChar
pattern QuoteChar_Single_quote = QuoteChar' "single_quote"

{-# COMPLETE
  QuoteChar_Disabled,
  QuoteChar_Quillemet,
  QuoteChar_Quote,
  QuoteChar_Single_quote,
  QuoteChar'
  #-}
