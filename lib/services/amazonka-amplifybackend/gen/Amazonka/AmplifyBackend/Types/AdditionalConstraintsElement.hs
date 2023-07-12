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
-- Module      : Amazonka.AmplifyBackend.Types.AdditionalConstraintsElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.AdditionalConstraintsElement
  ( AdditionalConstraintsElement
      ( ..,
        AdditionalConstraintsElement_REQUIRE_DIGIT,
        AdditionalConstraintsElement_REQUIRE_LOWERCASE,
        AdditionalConstraintsElement_REQUIRE_SYMBOL,
        AdditionalConstraintsElement_REQUIRE_UPPERCASE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AdditionalConstraintsElement = AdditionalConstraintsElement'
  { fromAdditionalConstraintsElement ::
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

pattern AdditionalConstraintsElement_REQUIRE_DIGIT :: AdditionalConstraintsElement
pattern AdditionalConstraintsElement_REQUIRE_DIGIT = AdditionalConstraintsElement' "REQUIRE_DIGIT"

pattern AdditionalConstraintsElement_REQUIRE_LOWERCASE :: AdditionalConstraintsElement
pattern AdditionalConstraintsElement_REQUIRE_LOWERCASE = AdditionalConstraintsElement' "REQUIRE_LOWERCASE"

pattern AdditionalConstraintsElement_REQUIRE_SYMBOL :: AdditionalConstraintsElement
pattern AdditionalConstraintsElement_REQUIRE_SYMBOL = AdditionalConstraintsElement' "REQUIRE_SYMBOL"

pattern AdditionalConstraintsElement_REQUIRE_UPPERCASE :: AdditionalConstraintsElement
pattern AdditionalConstraintsElement_REQUIRE_UPPERCASE = AdditionalConstraintsElement' "REQUIRE_UPPERCASE"

{-# COMPLETE
  AdditionalConstraintsElement_REQUIRE_DIGIT,
  AdditionalConstraintsElement_REQUIRE_LOWERCASE,
  AdditionalConstraintsElement_REQUIRE_SYMBOL,
  AdditionalConstraintsElement_REQUIRE_UPPERCASE,
  AdditionalConstraintsElement'
  #-}
