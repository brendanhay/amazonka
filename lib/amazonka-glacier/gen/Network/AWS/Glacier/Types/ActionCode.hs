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
        ActionCodeArchiveRetrieval,
        ActionCodeInventoryRetrieval,
        ActionCodeSelect,
        fromActionCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActionCode = ActionCode' {fromActionCode :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ActionCodeArchiveRetrieval :: ActionCode
pattern ActionCodeArchiveRetrieval = ActionCode' "ArchiveRetrieval"

pattern ActionCodeInventoryRetrieval :: ActionCode
pattern ActionCodeInventoryRetrieval = ActionCode' "InventoryRetrieval"

pattern ActionCodeSelect :: ActionCode
pattern ActionCodeSelect = ActionCode' "Select"

{-# COMPLETE
  ActionCodeArchiveRetrieval,
  ActionCodeInventoryRetrieval,
  ActionCodeSelect,
  ActionCode'
  #-}
