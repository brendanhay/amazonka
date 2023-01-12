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
-- Module      : Amazonka.Redshift.Types.ScheduledActionTypeValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ScheduledActionTypeValues
  ( ScheduledActionTypeValues
      ( ..,
        ScheduledActionTypeValues_PauseCluster,
        ScheduledActionTypeValues_ResizeCluster,
        ScheduledActionTypeValues_ResumeCluster
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype ScheduledActionTypeValues = ScheduledActionTypeValues'
  { fromScheduledActionTypeValues ::
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
