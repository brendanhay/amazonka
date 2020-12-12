{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Compatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Compatibility
  ( Compatibility
      ( Compatibility',
        CBackward,
        CBackwardAll,
        CDisabled,
        CForward,
        CForwardAll,
        CFull,
        CFullAll,
        CNone
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

pattern CBackward :: Compatibility
pattern CBackward = Compatibility' "BACKWARD"

pattern CBackwardAll :: Compatibility
pattern CBackwardAll = Compatibility' "BACKWARD_ALL"

pattern CDisabled :: Compatibility
pattern CDisabled = Compatibility' "DISABLED"

pattern CForward :: Compatibility
pattern CForward = Compatibility' "FORWARD"

pattern CForwardAll :: Compatibility
pattern CForwardAll = Compatibility' "FORWARD_ALL"

pattern CFull :: Compatibility
pattern CFull = Compatibility' "FULL"

pattern CFullAll :: Compatibility
pattern CFullAll = Compatibility' "FULL_ALL"

pattern CNone :: Compatibility
pattern CNone = Compatibility' "NONE"

{-# COMPLETE
  CBackward,
  CBackwardAll,
  CDisabled,
  CForward,
  CForwardAll,
  CFull,
  CFullAll,
  CNone,
  Compatibility'
  #-}
