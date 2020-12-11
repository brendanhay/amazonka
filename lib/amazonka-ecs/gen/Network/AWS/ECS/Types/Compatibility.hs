-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Compatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Compatibility
  ( Compatibility
      ( Compatibility',
        CEC2,
        CFargate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Compatibility = Compatibility' Lude.Text
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

pattern CEC2 :: Compatibility
pattern CEC2 = Compatibility' "EC2"

pattern CFargate :: Compatibility
pattern CFargate = Compatibility' "FARGATE"

{-# COMPLETE
  CEC2,
  CFargate,
  Compatibility'
  #-}
