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
        Index,
        LeafNode,
        Node,
        Policy
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

pattern Index :: ObjectType
pattern Index = ObjectType' "INDEX"

pattern LeafNode :: ObjectType
pattern LeafNode = ObjectType' "LEAF_NODE"

pattern Node :: ObjectType
pattern Node = ObjectType' "NODE"

pattern Policy :: ObjectType
pattern Policy = ObjectType' "POLICY"

{-# COMPLETE
  Index,
  LeafNode,
  Node,
  Policy,
  ObjectType'
  #-}
