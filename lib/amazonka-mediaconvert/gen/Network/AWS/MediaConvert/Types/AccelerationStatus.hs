{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AccelerationStatus
  ( AccelerationStatus
    ( AccelerationStatus'
    , AccelerationStatusNotApplicable
    , AccelerationStatusInProgress
    , AccelerationStatusAccelerated
    , AccelerationStatusNotAccelerated
    , fromAccelerationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Describes whether the current job is running with accelerated transcoding. For jobs that have Acceleration (AccelerationMode) set to DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that have Acceleration (AccelerationMode) set to ENABLED or PREFERRED, AccelerationStatus is one of the other states. AccelerationStatus is IN_PROGRESS initially, while the service determines whether the input files and job settings are compatible with accelerated transcoding. If they are, AcclerationStatus is ACCELERATED. If your input files and job settings aren't compatible with accelerated transcoding, the service either fails your job or runs it without accelerated transcoding, depending on how you set Acceleration (AccelerationMode). When the service runs your job without accelerated transcoding, AccelerationStatus is NOT_ACCELERATED.
newtype AccelerationStatus = AccelerationStatus'{fromAccelerationStatus
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern AccelerationStatusNotApplicable :: AccelerationStatus
pattern AccelerationStatusNotApplicable = AccelerationStatus' "NOT_APPLICABLE"

pattern AccelerationStatusInProgress :: AccelerationStatus
pattern AccelerationStatusInProgress = AccelerationStatus' "IN_PROGRESS"

pattern AccelerationStatusAccelerated :: AccelerationStatus
pattern AccelerationStatusAccelerated = AccelerationStatus' "ACCELERATED"

pattern AccelerationStatusNotAccelerated :: AccelerationStatus
pattern AccelerationStatusNotAccelerated = AccelerationStatus' "NOT_ACCELERATED"

{-# COMPLETE 
  AccelerationStatusNotApplicable,

  AccelerationStatusInProgress,

  AccelerationStatusAccelerated,

  AccelerationStatusNotAccelerated,
  AccelerationStatus'
  #-}
