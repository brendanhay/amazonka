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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentHealth
  ( EnvironmentHealth
      ( ..,
        EnvironmentHealth_Green,
        EnvironmentHealth_Grey,
        EnvironmentHealth_Red,
        EnvironmentHealth_Yellow
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentHealth = EnvironmentHealth'
  { fromEnvironmentHealth ::
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
