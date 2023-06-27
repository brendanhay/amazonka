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
-- Module      : Amazonka.QuickSight.Types.PropertyUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PropertyUsage
  ( PropertyUsage
      ( ..,
        PropertyUsage_DIMENSION,
        PropertyUsage_INHERIT,
        PropertyUsage_MEASURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PropertyUsage = PropertyUsage'
  { fromPropertyUsage ::
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

pattern PropertyUsage_DIMENSION :: PropertyUsage
pattern PropertyUsage_DIMENSION = PropertyUsage' "DIMENSION"

pattern PropertyUsage_INHERIT :: PropertyUsage
pattern PropertyUsage_INHERIT = PropertyUsage' "INHERIT"

pattern PropertyUsage_MEASURE :: PropertyUsage
pattern PropertyUsage_MEASURE = PropertyUsage' "MEASURE"

{-# COMPLETE
  PropertyUsage_DIMENSION,
  PropertyUsage_INHERIT,
  PropertyUsage_MEASURE,
  PropertyUsage'
  #-}
