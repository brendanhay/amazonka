{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ActionCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ActionCode
  ( ActionCode
      ( ActionCode',
        ArchiveRetrieval,
        InventoryRetrieval,
        Select
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionCode = ActionCode' Lude.Text
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

pattern ArchiveRetrieval :: ActionCode
pattern ArchiveRetrieval = ActionCode' "ArchiveRetrieval"

pattern InventoryRetrieval :: ActionCode
pattern InventoryRetrieval = ActionCode' "InventoryRetrieval"

pattern Select :: ActionCode
pattern Select = ActionCode' "Select"

{-# COMPLETE
  ArchiveRetrieval,
  InventoryRetrieval,
  Select,
  ActionCode'
  #-}
