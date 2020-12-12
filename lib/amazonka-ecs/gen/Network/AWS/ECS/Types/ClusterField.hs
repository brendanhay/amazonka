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
        Attachments,
        Settings,
        Statistics,
        Tags
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

pattern Attachments :: ClusterField
pattern Attachments = ClusterField' "ATTACHMENTS"

pattern Settings :: ClusterField
pattern Settings = ClusterField' "SETTINGS"

pattern Statistics :: ClusterField
pattern Statistics = ClusterField' "STATISTICS"

pattern Tags :: ClusterField
pattern Tags = ClusterField' "TAGS"

{-# COMPLETE
  Attachments,
  Settings,
  Statistics,
  Tags,
  ClusterField'
  #-}
