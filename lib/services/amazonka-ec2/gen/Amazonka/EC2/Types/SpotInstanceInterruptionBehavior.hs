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
-- Module      : Amazonka.EC2.Types.SpotInstanceInterruptionBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotInstanceInterruptionBehavior
  ( SpotInstanceInterruptionBehavior
      ( ..,
        SpotInstanceInterruptionBehavior_Hibernate,
        SpotInstanceInterruptionBehavior_Stop,
        SpotInstanceInterruptionBehavior_Terminate
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype SpotInstanceInterruptionBehavior = SpotInstanceInterruptionBehavior'
  { fromSpotInstanceInterruptionBehavior ::
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

pattern SpotInstanceInterruptionBehavior_Hibernate :: SpotInstanceInterruptionBehavior
pattern SpotInstanceInterruptionBehavior_Hibernate = SpotInstanceInterruptionBehavior' "hibernate"

pattern SpotInstanceInterruptionBehavior_Stop :: SpotInstanceInterruptionBehavior
pattern SpotInstanceInterruptionBehavior_Stop = SpotInstanceInterruptionBehavior' "stop"

pattern SpotInstanceInterruptionBehavior_Terminate :: SpotInstanceInterruptionBehavior
pattern SpotInstanceInterruptionBehavior_Terminate = SpotInstanceInterruptionBehavior' "terminate"

{-# COMPLETE
  SpotInstanceInterruptionBehavior_Hibernate,
  SpotInstanceInterruptionBehavior_Stop,
  SpotInstanceInterruptionBehavior_Terminate,
  SpotInstanceInterruptionBehavior'
  #-}
