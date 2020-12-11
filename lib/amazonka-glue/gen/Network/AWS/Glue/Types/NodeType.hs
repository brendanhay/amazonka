-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.NodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.NodeType
  ( NodeType
      ( NodeType',
        Crawler,
        Job,
        Trigger
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NodeType = NodeType' Lude.Text
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

pattern Crawler :: NodeType
pattern Crawler = NodeType' "CRAWLER"

pattern Job :: NodeType
pattern Job = NodeType' "JOB"

pattern Trigger :: NodeType
pattern Trigger = NodeType' "TRIGGER"

{-# COMPLETE
  Crawler,
  Job,
  Trigger,
  NodeType'
  #-}
