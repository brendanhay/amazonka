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
-- Module      : Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
  ( SpotInstanceInterruptionBehavior
      ( ..,
        SpotInstanceInterruptionBehavior_Hibernate,
        SpotInstanceInterruptionBehavior_Stop,
        SpotInstanceInterruptionBehavior_Terminate
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype SpotInstanceInterruptionBehavior = SpotInstanceInterruptionBehavior'
  { fromSpotInstanceInterruptionBehavior ::
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
