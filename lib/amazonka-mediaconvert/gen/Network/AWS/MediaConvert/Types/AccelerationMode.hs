{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AccelerationMode
  ( AccelerationMode
    ( AccelerationMode'
    , AccelerationModeDisabled
    , AccelerationModeEnabled
    , AccelerationModePreferred
    , fromAccelerationMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify whether the service runs your job with accelerated transcoding. Choose DISABLED if you don't want accelerated transcoding. Choose ENABLED if you want your job to run with accelerated transcoding and to fail if your input files or your job settings aren't compatible with accelerated transcoding. Choose PREFERRED if you want your job to run with accelerated transcoding if the job is compatible with the feature and to run at standard speed if it's not.
newtype AccelerationMode = AccelerationMode'{fromAccelerationMode
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern AccelerationModeDisabled :: AccelerationMode
pattern AccelerationModeDisabled = AccelerationMode' "DISABLED"

pattern AccelerationModeEnabled :: AccelerationMode
pattern AccelerationModeEnabled = AccelerationMode' "ENABLED"

pattern AccelerationModePreferred :: AccelerationMode
pattern AccelerationModePreferred = AccelerationMode' "PREFERRED"

{-# COMPLETE 
  AccelerationModeDisabled,

  AccelerationModeEnabled,

  AccelerationModePreferred,
  AccelerationMode'
  #-}
