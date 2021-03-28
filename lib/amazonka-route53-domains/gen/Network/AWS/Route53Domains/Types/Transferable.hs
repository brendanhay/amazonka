{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Transferable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.Transferable
  ( Transferable
    ( Transferable'
    , TransferableTransferable
    , TransferableUntransferable
    , TransferableDontKnow
    , fromTransferable
    )
  ) where

import qualified Network.AWS.Prelude as Core

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
--
--
newtype Transferable = Transferable'{fromTransferable :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern TransferableTransferable :: Transferable
pattern TransferableTransferable = Transferable' "TRANSFERABLE"

pattern TransferableUntransferable :: Transferable
pattern TransferableUntransferable = Transferable' "UNTRANSFERABLE"

pattern TransferableDontKnow :: Transferable
pattern TransferableDontKnow = Transferable' "DONT_KNOW"

{-# COMPLETE 
  TransferableTransferable,

  TransferableUntransferable,

  TransferableDontKnow,
  Transferable'
  #-}
