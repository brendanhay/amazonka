{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3MetadataControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3MetadataControl
  ( Ac3MetadataControl
      ( Ac3MetadataControl',
        AMCFollowInput,
        AMCUseConfigured
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Ac3 Metadata Control
newtype Ac3MetadataControl = Ac3MetadataControl' Lude.Text
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

pattern AMCFollowInput :: Ac3MetadataControl
pattern AMCFollowInput = Ac3MetadataControl' "FOLLOW_INPUT"

pattern AMCUseConfigured :: Ac3MetadataControl
pattern AMCUseConfigured = Ac3MetadataControl' "USE_CONFIGURED"

{-# COMPLETE
  AMCFollowInput,
  AMCUseConfigured,
  Ac3MetadataControl'
  #-}
