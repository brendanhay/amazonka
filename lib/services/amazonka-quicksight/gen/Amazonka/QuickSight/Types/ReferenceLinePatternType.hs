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
-- Module      : Amazonka.QuickSight.Types.ReferenceLinePatternType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLinePatternType
  ( ReferenceLinePatternType
      ( ..,
        ReferenceLinePatternType_DASHED,
        ReferenceLinePatternType_DOTTED,
        ReferenceLinePatternType_SOLID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReferenceLinePatternType = ReferenceLinePatternType'
  { fromReferenceLinePatternType ::
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

pattern ReferenceLinePatternType_DASHED :: ReferenceLinePatternType
pattern ReferenceLinePatternType_DASHED = ReferenceLinePatternType' "DASHED"

pattern ReferenceLinePatternType_DOTTED :: ReferenceLinePatternType
pattern ReferenceLinePatternType_DOTTED = ReferenceLinePatternType' "DOTTED"

pattern ReferenceLinePatternType_SOLID :: ReferenceLinePatternType
pattern ReferenceLinePatternType_SOLID = ReferenceLinePatternType' "SOLID"

{-# COMPLETE
  ReferenceLinePatternType_DASHED,
  ReferenceLinePatternType_DOTTED,
  ReferenceLinePatternType_SOLID,
  ReferenceLinePatternType'
  #-}
