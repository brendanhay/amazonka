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
-- Module      : Network.AWS.Redshift.Types.ScheduledActionTypeValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionTypeValues
  ( ScheduledActionTypeValues
      ( ..,
        ScheduledActionTypeValues_PauseCluster,
        ScheduledActionTypeValues_ResizeCluster,
        ScheduledActionTypeValues_ResumeCluster
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype ScheduledActionTypeValues = ScheduledActionTypeValues'
  { fromScheduledActionTypeValues ::
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

pattern ScheduledActionTypeValues_PauseCluster :: ScheduledActionTypeValues
pattern ScheduledActionTypeValues_PauseCluster = ScheduledActionTypeValues' "PauseCluster"

pattern ScheduledActionTypeValues_ResizeCluster :: ScheduledActionTypeValues
pattern ScheduledActionTypeValues_ResizeCluster = ScheduledActionTypeValues' "ResizeCluster"

pattern ScheduledActionTypeValues_ResumeCluster :: ScheduledActionTypeValues
pattern ScheduledActionTypeValues_ResumeCluster = ScheduledActionTypeValues' "ResumeCluster"

{-# COMPLETE
  ScheduledActionTypeValues_PauseCluster,
  ScheduledActionTypeValues_ResizeCluster,
  ScheduledActionTypeValues_ResumeCluster,
  ScheduledActionTypeValues'
  #-}
