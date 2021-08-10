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
-- Module      : Network.AWS.RDS.Types.TargetState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetState
  ( TargetState
      ( ..,
        TargetState_AVAILABLE,
        TargetState_REGISTERING,
        TargetState_UNAVAILABLE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TargetState = TargetState'
  { fromTargetState ::
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

pattern TargetState_AVAILABLE :: TargetState
pattern TargetState_AVAILABLE = TargetState' "AVAILABLE"

pattern TargetState_REGISTERING :: TargetState
pattern TargetState_REGISTERING = TargetState' "REGISTERING"

pattern TargetState_UNAVAILABLE :: TargetState
pattern TargetState_UNAVAILABLE = TargetState' "UNAVAILABLE"

{-# COMPLETE
  TargetState_AVAILABLE,
  TargetState_REGISTERING,
  TargetState_UNAVAILABLE,
  TargetState'
  #-}
