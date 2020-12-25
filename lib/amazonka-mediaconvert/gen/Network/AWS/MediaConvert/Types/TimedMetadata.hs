{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimedMetadata
  ( TimedMetadata
      ( TimedMetadata',
        TimedMetadataPassthrough,
        TimedMetadataNone,
        fromTimedMetadata
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
newtype TimedMetadata = TimedMetadata'
  { fromTimedMetadata ::
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

pattern TimedMetadataPassthrough :: TimedMetadata
pattern TimedMetadataPassthrough = TimedMetadata' "PASSTHROUGH"

pattern TimedMetadataNone :: TimedMetadata
pattern TimedMetadataNone = TimedMetadata' "NONE"

{-# COMPLETE
  TimedMetadataPassthrough,
  TimedMetadataNone,
  TimedMetadata'
  #-}
