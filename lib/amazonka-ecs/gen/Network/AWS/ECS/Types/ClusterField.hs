{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ClusterField
  ( ClusterField
      ( ClusterField',
        CFAttachments,
        CFSettings,
        CFStatistics,
        CFTags
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ClusterField = ClusterField' Lude.Text
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

pattern CFAttachments :: ClusterField
pattern CFAttachments = ClusterField' "ATTACHMENTS"

pattern CFSettings :: ClusterField
pattern CFSettings = ClusterField' "SETTINGS"

pattern CFStatistics :: ClusterField
pattern CFStatistics = ClusterField' "STATISTICS"

pattern CFTags :: ClusterField
pattern CFTags = ClusterField' "TAGS"

{-# COMPLETE
  CFAttachments,
  CFSettings,
  CFStatistics,
  CFTags,
  ClusterField'
  #-}
