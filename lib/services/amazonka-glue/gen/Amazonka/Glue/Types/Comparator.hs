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
-- Module      : Amazonka.Glue.Types.Comparator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Comparator
  ( Comparator
      ( ..,
        Comparator_EQUALS,
        Comparator_GREATER_THAN,
        Comparator_GREATER_THAN_EQUALS,
        Comparator_LESS_THAN,
        Comparator_LESS_THAN_EQUALS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Comparator = Comparator'
  { fromComparator ::
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

pattern Comparator_EQUALS :: Comparator
pattern Comparator_EQUALS = Comparator' "EQUALS"

pattern Comparator_GREATER_THAN :: Comparator
pattern Comparator_GREATER_THAN = Comparator' "GREATER_THAN"

pattern Comparator_GREATER_THAN_EQUALS :: Comparator
pattern Comparator_GREATER_THAN_EQUALS = Comparator' "GREATER_THAN_EQUALS"

pattern Comparator_LESS_THAN :: Comparator
pattern Comparator_LESS_THAN = Comparator' "LESS_THAN"

pattern Comparator_LESS_THAN_EQUALS :: Comparator
pattern Comparator_LESS_THAN_EQUALS = Comparator' "LESS_THAN_EQUALS"

{-# COMPLETE
  Comparator_EQUALS,
  Comparator_GREATER_THAN,
  Comparator_GREATER_THAN_EQUALS,
  Comparator_LESS_THAN,
  Comparator_LESS_THAN_EQUALS,
  Comparator'
  #-}
