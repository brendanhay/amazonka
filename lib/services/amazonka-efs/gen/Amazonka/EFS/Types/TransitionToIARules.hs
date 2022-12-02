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
-- Module      : Amazonka.EFS.Types.TransitionToIARules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.TransitionToIARules
  ( TransitionToIARules
      ( ..,
        TransitionToIARules_AFTER_14_DAYS,
        TransitionToIARules_AFTER_30_DAYS,
        TransitionToIARules_AFTER_60_DAYS,
        TransitionToIARules_AFTER_7_DAYS,
        TransitionToIARules_AFTER_90_DAYS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransitionToIARules = TransitionToIARules'
  { fromTransitionToIARules ::
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

pattern TransitionToIARules_AFTER_14_DAYS :: TransitionToIARules
pattern TransitionToIARules_AFTER_14_DAYS = TransitionToIARules' "AFTER_14_DAYS"

pattern TransitionToIARules_AFTER_30_DAYS :: TransitionToIARules
pattern TransitionToIARules_AFTER_30_DAYS = TransitionToIARules' "AFTER_30_DAYS"

pattern TransitionToIARules_AFTER_60_DAYS :: TransitionToIARules
pattern TransitionToIARules_AFTER_60_DAYS = TransitionToIARules' "AFTER_60_DAYS"

pattern TransitionToIARules_AFTER_7_DAYS :: TransitionToIARules
pattern TransitionToIARules_AFTER_7_DAYS = TransitionToIARules' "AFTER_7_DAYS"

pattern TransitionToIARules_AFTER_90_DAYS :: TransitionToIARules
pattern TransitionToIARules_AFTER_90_DAYS = TransitionToIARules' "AFTER_90_DAYS"

{-# COMPLETE
  TransitionToIARules_AFTER_14_DAYS,
  TransitionToIARules_AFTER_30_DAYS,
  TransitionToIARules_AFTER_60_DAYS,
  TransitionToIARules_AFTER_7_DAYS,
  TransitionToIARules_AFTER_90_DAYS,
  TransitionToIARules'
  #-}
