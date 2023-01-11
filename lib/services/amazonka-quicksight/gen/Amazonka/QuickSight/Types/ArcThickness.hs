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
-- Module      : Amazonka.QuickSight.Types.ArcThickness
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ArcThickness
  ( ArcThickness
      ( ..,
        ArcThickness_LARGE,
        ArcThickness_MEDIUM,
        ArcThickness_SMALL,
        ArcThickness_WHOLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArcThickness = ArcThickness'
  { fromArcThickness ::
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

pattern ArcThickness_LARGE :: ArcThickness
pattern ArcThickness_LARGE = ArcThickness' "LARGE"

pattern ArcThickness_MEDIUM :: ArcThickness
pattern ArcThickness_MEDIUM = ArcThickness' "MEDIUM"

pattern ArcThickness_SMALL :: ArcThickness
pattern ArcThickness_SMALL = ArcThickness' "SMALL"

pattern ArcThickness_WHOLE :: ArcThickness
pattern ArcThickness_WHOLE = ArcThickness' "WHOLE"

{-# COMPLETE
  ArcThickness_LARGE,
  ArcThickness_MEDIUM,
  ArcThickness_SMALL,
  ArcThickness_WHOLE,
  ArcThickness'
  #-}
