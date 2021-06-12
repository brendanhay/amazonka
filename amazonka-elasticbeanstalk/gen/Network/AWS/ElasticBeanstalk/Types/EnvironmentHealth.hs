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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
  ( EnvironmentHealth
      ( ..,
        EnvironmentHealth_Green,
        EnvironmentHealth_Grey,
        EnvironmentHealth_Red,
        EnvironmentHealth_Yellow
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EnvironmentHealth = EnvironmentHealth'
  { fromEnvironmentHealth ::
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

pattern EnvironmentHealth_Green :: EnvironmentHealth
pattern EnvironmentHealth_Green = EnvironmentHealth' "Green"

pattern EnvironmentHealth_Grey :: EnvironmentHealth
pattern EnvironmentHealth_Grey = EnvironmentHealth' "Grey"

pattern EnvironmentHealth_Red :: EnvironmentHealth
pattern EnvironmentHealth_Red = EnvironmentHealth' "Red"

pattern EnvironmentHealth_Yellow :: EnvironmentHealth
pattern EnvironmentHealth_Yellow = EnvironmentHealth' "Yellow"

{-# COMPLETE
  EnvironmentHealth_Green,
  EnvironmentHealth_Grey,
  EnvironmentHealth_Red,
  EnvironmentHealth_Yellow,
  EnvironmentHealth'
  #-}
