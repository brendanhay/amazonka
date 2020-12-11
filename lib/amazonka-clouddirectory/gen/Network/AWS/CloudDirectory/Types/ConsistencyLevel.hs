-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ConsistencyLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ConsistencyLevel
  ( ConsistencyLevel
      ( ConsistencyLevel',
        Eventual,
        Serializable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConsistencyLevel = ConsistencyLevel' Lude.Text
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

pattern Eventual :: ConsistencyLevel
pattern Eventual = ConsistencyLevel' "EVENTUAL"

pattern Serializable :: ConsistencyLevel
pattern Serializable = ConsistencyLevel' "SERIALIZABLE"

{-# COMPLETE
  Eventual,
  Serializable,
  ConsistencyLevel'
  #-}
