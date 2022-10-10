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
-- Module      : Amazonka.ECS.Types.DesiredStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DesiredStatus
  ( DesiredStatus
      ( ..,
        DesiredStatus_PENDING,
        DesiredStatus_RUNNING,
        DesiredStatus_STOPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DesiredStatus = DesiredStatus'
  { fromDesiredStatus ::
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

pattern DesiredStatus_PENDING :: DesiredStatus
pattern DesiredStatus_PENDING = DesiredStatus' "PENDING"

pattern DesiredStatus_RUNNING :: DesiredStatus
pattern DesiredStatus_RUNNING = DesiredStatus' "RUNNING"

pattern DesiredStatus_STOPPED :: DesiredStatus
pattern DesiredStatus_STOPPED = DesiredStatus' "STOPPED"

{-# COMPLETE
  DesiredStatus_PENDING,
  DesiredStatus_RUNNING,
  DesiredStatus_STOPPED,
  DesiredStatus'
  #-}
