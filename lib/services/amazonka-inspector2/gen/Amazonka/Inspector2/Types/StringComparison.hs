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
-- Module      : Amazonka.Inspector2.Types.StringComparison
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.StringComparison
  ( StringComparison
      ( ..,
        StringComparison_EQUALS,
        StringComparison_NOT_EQUALS,
        StringComparison_PREFIX
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StringComparison = StringComparison'
  { fromStringComparison ::
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

pattern StringComparison_EQUALS :: StringComparison
pattern StringComparison_EQUALS = StringComparison' "EQUALS"

pattern StringComparison_NOT_EQUALS :: StringComparison
pattern StringComparison_NOT_EQUALS = StringComparison' "NOT_EQUALS"

pattern StringComparison_PREFIX :: StringComparison
pattern StringComparison_PREFIX = StringComparison' "PREFIX"

{-# COMPLETE
  StringComparison_EQUALS,
  StringComparison_NOT_EQUALS,
  StringComparison_PREFIX,
  StringComparison'
  #-}
