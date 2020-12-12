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
        None,
        Passthrough,
        Respond
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Video Description Respond To Afd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd' Lude.Text
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

pattern None :: VideoDescriptionRespondToAfd
pattern None = VideoDescriptionRespondToAfd' "NONE"

pattern Passthrough :: VideoDescriptionRespondToAfd
pattern Passthrough = VideoDescriptionRespondToAfd' "PASSTHROUGH"

pattern Respond :: VideoDescriptionRespondToAfd
pattern Respond = VideoDescriptionRespondToAfd' "RESPOND"

{-# COMPLETE
  None,
  Passthrough,
  Respond,
  VideoDescriptionRespondToAfd'
  #-}
