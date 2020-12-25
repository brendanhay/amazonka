{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
  ( EnvironmentHealth
      ( EnvironmentHealth',
        EnvironmentHealthGreen,
        EnvironmentHealthYellow,
        EnvironmentHealthRed,
        EnvironmentHealthGrey,
        fromEnvironmentHealth
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EnvironmentHealth = EnvironmentHealth'
  { fromEnvironmentHealth ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern EnvironmentHealthGreen :: EnvironmentHealth
pattern EnvironmentHealthGreen = EnvironmentHealth' "Green"

pattern EnvironmentHealthYellow :: EnvironmentHealth
pattern EnvironmentHealthYellow = EnvironmentHealth' "Yellow"

pattern EnvironmentHealthRed :: EnvironmentHealth
pattern EnvironmentHealthRed = EnvironmentHealth' "Red"

pattern EnvironmentHealthGrey :: EnvironmentHealth
pattern EnvironmentHealthGrey = EnvironmentHealth' "Grey"

{-# COMPLETE
  EnvironmentHealthGreen,
  EnvironmentHealthYellow,
  EnvironmentHealthRed,
  EnvironmentHealthGrey,
  EnvironmentHealth'
  #-}
