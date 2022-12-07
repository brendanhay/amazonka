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
-- Module      : Amazonka.EC2.Types.InstanceInterruptionBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceInterruptionBehavior
  ( InstanceInterruptionBehavior
      ( ..,
        InstanceInterruptionBehavior_Hibernate,
        InstanceInterruptionBehavior_Stop,
        InstanceInterruptionBehavior_Terminate
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype InstanceInterruptionBehavior = InstanceInterruptionBehavior'
  { fromInstanceInterruptionBehavior ::
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

pattern InstanceInterruptionBehavior_Hibernate :: InstanceInterruptionBehavior
pattern InstanceInterruptionBehavior_Hibernate = InstanceInterruptionBehavior' "hibernate"

pattern InstanceInterruptionBehavior_Stop :: InstanceInterruptionBehavior
pattern InstanceInterruptionBehavior_Stop = InstanceInterruptionBehavior' "stop"

pattern InstanceInterruptionBehavior_Terminate :: InstanceInterruptionBehavior
pattern InstanceInterruptionBehavior_Terminate = InstanceInterruptionBehavior' "terminate"

{-# COMPLETE
  InstanceInterruptionBehavior_Hibernate,
  InstanceInterruptionBehavior_Stop,
  InstanceInterruptionBehavior_Terminate,
  InstanceInterruptionBehavior'
  #-}
