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
-- Module      : Amazonka.ResilienceHub.Types.DataLocationConstraint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.DataLocationConstraint
  ( DataLocationConstraint
      ( ..,
        DataLocationConstraint_AnyLocation,
        DataLocationConstraint_SameContinent,
        DataLocationConstraint_SameCountry
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataLocationConstraint = DataLocationConstraint'
  { fromDataLocationConstraint ::
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

pattern DataLocationConstraint_AnyLocation :: DataLocationConstraint
pattern DataLocationConstraint_AnyLocation = DataLocationConstraint' "AnyLocation"

pattern DataLocationConstraint_SameContinent :: DataLocationConstraint
pattern DataLocationConstraint_SameContinent = DataLocationConstraint' "SameContinent"

pattern DataLocationConstraint_SameCountry :: DataLocationConstraint
pattern DataLocationConstraint_SameCountry = DataLocationConstraint' "SameCountry"

{-# COMPLETE
  DataLocationConstraint_AnyLocation,
  DataLocationConstraint_SameContinent,
  DataLocationConstraint_SameCountry,
  DataLocationConstraint'
  #-}
