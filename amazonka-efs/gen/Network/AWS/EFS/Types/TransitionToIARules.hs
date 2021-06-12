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
-- Module      : Network.AWS.EFS.Types.TransitionToIARules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.TransitionToIARules
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

import qualified Network.AWS.Core as Core

newtype TransitionToIARules = TransitionToIARules'
  { fromTransitionToIARules ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
