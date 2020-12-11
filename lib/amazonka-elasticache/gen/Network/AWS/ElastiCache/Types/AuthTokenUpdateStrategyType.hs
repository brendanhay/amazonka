-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
  ( AuthTokenUpdateStrategyType
      ( AuthTokenUpdateStrategyType',
        Delete,
        Rotate,
        Set
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuthTokenUpdateStrategyType = AuthTokenUpdateStrategyType' Lude.Text
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

pattern Delete :: AuthTokenUpdateStrategyType
pattern Delete = AuthTokenUpdateStrategyType' "DELETE"

pattern Rotate :: AuthTokenUpdateStrategyType
pattern Rotate = AuthTokenUpdateStrategyType' "ROTATE"

pattern Set :: AuthTokenUpdateStrategyType
pattern Set = AuthTokenUpdateStrategyType' "SET"

{-# COMPLETE
  Delete,
  Rotate,
  Set,
  AuthTokenUpdateStrategyType'
  #-}
