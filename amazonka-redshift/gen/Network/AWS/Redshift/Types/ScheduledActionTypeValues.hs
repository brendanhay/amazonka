{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype ScheduledActionTypeValues = ScheduledActionTypeValues'
  { fromScheduledActionTypeValues ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
