{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectType
  ( ObjectType
      ( ObjectType',
        Node,
        LeafNode,
        Policy,
        Index
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ObjectType = ObjectType' Lude.Text
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

pattern Node :: ObjectType
pattern Node = ObjectType' "NODE"

pattern LeafNode :: ObjectType
pattern LeafNode = ObjectType' "LEAF_NODE"

pattern Policy :: ObjectType
pattern Policy = ObjectType' "POLICY"

pattern Index :: ObjectType
pattern Index = ObjectType' "INDEX"

{-# COMPLETE
  Node,
  LeafNode,
  Policy,
  Index,
  ObjectType'
  #-}
