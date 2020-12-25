{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
  ( VideoDescriptionRespondToAfd
      ( VideoDescriptionRespondToAfd',
        VideoDescriptionRespondToAfdNone,
        VideoDescriptionRespondToAfdPassthrough,
        VideoDescriptionRespondToAfdRespond,
        fromVideoDescriptionRespondToAfd
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Video Description Respond To Afd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd'
  { fromVideoDescriptionRespondToAfd ::
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

pattern VideoDescriptionRespondToAfdNone :: VideoDescriptionRespondToAfd
pattern VideoDescriptionRespondToAfdNone = VideoDescriptionRespondToAfd' "NONE"

pattern VideoDescriptionRespondToAfdPassthrough :: VideoDescriptionRespondToAfd
pattern VideoDescriptionRespondToAfdPassthrough = VideoDescriptionRespondToAfd' "PASSTHROUGH"

pattern VideoDescriptionRespondToAfdRespond :: VideoDescriptionRespondToAfd
pattern VideoDescriptionRespondToAfdRespond = VideoDescriptionRespondToAfd' "RESPOND"

{-# COMPLETE
  VideoDescriptionRespondToAfdNone,
  VideoDescriptionRespondToAfdPassthrough,
  VideoDescriptionRespondToAfdRespond,
  VideoDescriptionRespondToAfd'
  #-}
