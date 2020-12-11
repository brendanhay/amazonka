-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkMode
  ( NetworkMode
      ( NetworkMode',
        AWSvpc,
        Bridge,
        Host,
        None
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NetworkMode = NetworkMode' Lude.Text
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

pattern AWSvpc :: NetworkMode
pattern AWSvpc = NetworkMode' "awsvpc"

pattern Bridge :: NetworkMode
pattern Bridge = NetworkMode' "bridge"

pattern Host :: NetworkMode
pattern Host = NetworkMode' "host"

pattern None :: NetworkMode
pattern None = NetworkMode' "none"

{-# COMPLETE
  AWSvpc,
  Bridge,
  Host,
  None,
  NetworkMode'
  #-}
