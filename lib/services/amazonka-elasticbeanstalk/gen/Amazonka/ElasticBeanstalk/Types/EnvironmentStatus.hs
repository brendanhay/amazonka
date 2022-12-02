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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentStatus
  ( EnvironmentStatus
      ( ..,
        EnvironmentStatus_Aborting,
        EnvironmentStatus_Launching,
        EnvironmentStatus_LinkingFrom,
        EnvironmentStatus_LinkingTo,
        EnvironmentStatus_Ready,
        EnvironmentStatus_Terminated,
        EnvironmentStatus_Terminating,
        EnvironmentStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentStatus = EnvironmentStatus'
  { fromEnvironmentStatus ::
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

pattern EnvironmentStatus_Aborting :: EnvironmentStatus
pattern EnvironmentStatus_Aborting = EnvironmentStatus' "Aborting"

pattern EnvironmentStatus_Launching :: EnvironmentStatus
pattern EnvironmentStatus_Launching = EnvironmentStatus' "Launching"

pattern EnvironmentStatus_LinkingFrom :: EnvironmentStatus
pattern EnvironmentStatus_LinkingFrom = EnvironmentStatus' "LinkingFrom"

pattern EnvironmentStatus_LinkingTo :: EnvironmentStatus
pattern EnvironmentStatus_LinkingTo = EnvironmentStatus' "LinkingTo"

pattern EnvironmentStatus_Ready :: EnvironmentStatus
pattern EnvironmentStatus_Ready = EnvironmentStatus' "Ready"

pattern EnvironmentStatus_Terminated :: EnvironmentStatus
pattern EnvironmentStatus_Terminated = EnvironmentStatus' "Terminated"

pattern EnvironmentStatus_Terminating :: EnvironmentStatus
pattern EnvironmentStatus_Terminating = EnvironmentStatus' "Terminating"

pattern EnvironmentStatus_Updating :: EnvironmentStatus
pattern EnvironmentStatus_Updating = EnvironmentStatus' "Updating"

{-# COMPLETE
  EnvironmentStatus_Aborting,
  EnvironmentStatus_Launching,
  EnvironmentStatus_LinkingFrom,
  EnvironmentStatus_LinkingTo,
  EnvironmentStatus_Ready,
  EnvironmentStatus_Terminated,
  EnvironmentStatus_Terminating,
  EnvironmentStatus_Updating,
  EnvironmentStatus'
  #-}
