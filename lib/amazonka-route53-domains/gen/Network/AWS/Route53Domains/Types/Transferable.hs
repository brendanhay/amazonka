-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Transferable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Transferable
  ( Transferable
      ( Transferable',
        DontKnow,
        Transferable,
        Untransferable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Whether the domain name can be transferred to Route 53.
--
-- Valid values:
--
--     * TRANSFERABLE
--
--     * The domain name can be transferred to Route 53.
--
--
--     * UNTRANSFERRABLE
--
--     * The domain name can't be transferred to Route 53.
--
--
--     * DONT_KNOW
--
--     * Reserved for future use.
newtype Transferable = Transferable' Lude.Text
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

pattern DontKnow :: Transferable
pattern DontKnow = Transferable' "DONT_KNOW"

pattern Transferable :: Transferable
pattern Transferable = Transferable' "TRANSFERABLE"

pattern Untransferable :: Transferable
pattern Untransferable = Transferable' "UNTRANSFERABLE"

{-# COMPLETE
  DontKnow,
  Transferable,
  Untransferable,
  Transferable'
  #-}
