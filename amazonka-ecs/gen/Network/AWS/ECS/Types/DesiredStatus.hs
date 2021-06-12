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
-- Module      : Network.AWS.ECS.Types.DesiredStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DesiredStatus
  ( DesiredStatus
      ( ..,
        DesiredStatus_PENDING,
        DesiredStatus_RUNNING,
        DesiredStatus_STOPPED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DesiredStatus = DesiredStatus'
  { fromDesiredStatus ::
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
