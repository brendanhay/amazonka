{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Distribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.Distribution
  ( Distribution
      ( Distribution',
        DistributionRandom,
        DistributionByLogStream,
        fromDistribution
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The method used to distribute log data to the destination, which can be either random or grouped by log stream.
newtype Distribution = Distribution' {fromDistribution :: Core.Text}
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

pattern DistributionRandom :: Distribution
pattern DistributionRandom = Distribution' "Random"

pattern DistributionByLogStream :: Distribution
pattern DistributionByLogStream = Distribution' "ByLogStream"

{-# COMPLETE
  DistributionRandom,
  DistributionByLogStream,
  Distribution'
  #-}
