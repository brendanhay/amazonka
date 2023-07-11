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
-- Module      : Amazonka.MigrationHubStrategy.Types.Condition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Condition
  ( Condition
      ( ..,
        Condition_CONTAINS,
        Condition_EQUALS,
        Condition_NOT_CONTAINS,
        Condition_NOT_EQUALS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Condition = Condition'
  { fromCondition ::
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

pattern Condition_CONTAINS :: Condition
pattern Condition_CONTAINS = Condition' "CONTAINS"

pattern Condition_EQUALS :: Condition
pattern Condition_EQUALS = Condition' "EQUALS"

pattern Condition_NOT_CONTAINS :: Condition
pattern Condition_NOT_CONTAINS = Condition' "NOT_CONTAINS"

pattern Condition_NOT_EQUALS :: Condition
pattern Condition_NOT_EQUALS = Condition' "NOT_EQUALS"

{-# COMPLETE
  Condition_CONTAINS,
  Condition_EQUALS,
  Condition_NOT_CONTAINS,
  Condition_NOT_EQUALS,
  Condition'
  #-}
