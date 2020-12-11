-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationMode
  ( AccelerationMode
      ( AccelerationMode',
        AMDisabled,
        AMEnabled,
        AMPreferred
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether the service runs your job with accelerated transcoding. Choose DISABLED if you don't want accelerated transcoding. Choose ENABLED if you want your job to run with accelerated transcoding and to fail if your input files or your job settings aren't compatible with accelerated transcoding. Choose PREFERRED if you want your job to run with accelerated transcoding if the job is compatible with the feature and to run at standard speed if it's not.
newtype AccelerationMode = AccelerationMode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AMDisabled :: AccelerationMode
pattern AMDisabled = AccelerationMode' "DISABLED"

pattern AMEnabled :: AccelerationMode
pattern AMEnabled = AccelerationMode' "ENABLED"

pattern AMPreferred :: AccelerationMode
pattern AMPreferred = AccelerationMode' "PREFERRED"

{-# COMPLETE
  AMDisabled,
  AMEnabled,
  AMPreferred,
  AccelerationMode'
  #-}
