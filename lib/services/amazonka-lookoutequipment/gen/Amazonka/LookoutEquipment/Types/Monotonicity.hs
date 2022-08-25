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
-- Module      : Amazonka.LookoutEquipment.Types.Monotonicity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.Monotonicity
  ( Monotonicity
      ( ..,
        Monotonicity_DECREASING,
        Monotonicity_INCREASING,
        Monotonicity_STATIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Monotonicity = Monotonicity'
  { fromMonotonicity ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Monotonicity_DECREASING :: Monotonicity
pattern Monotonicity_DECREASING = Monotonicity' "DECREASING"

pattern Monotonicity_INCREASING :: Monotonicity
pattern Monotonicity_INCREASING = Monotonicity' "INCREASING"

pattern Monotonicity_STATIC :: Monotonicity
pattern Monotonicity_STATIC = Monotonicity' "STATIC"

{-# COMPLETE
  Monotonicity_DECREASING,
  Monotonicity_INCREASING,
  Monotonicity_STATIC,
  Monotonicity'
  #-}
