{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRouteStatusMsg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IPRouteStatusMsg
  ( IPRouteStatusMsg
      ( IPRouteStatusMsg',
        AddFailed,
        Added,
        Adding,
        RemoveFailed,
        Removed,
        Removing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IPRouteStatusMsg = IPRouteStatusMsg' Lude.Text
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

pattern AddFailed :: IPRouteStatusMsg
pattern AddFailed = IPRouteStatusMsg' "AddFailed"

pattern Added :: IPRouteStatusMsg
pattern Added = IPRouteStatusMsg' "Added"

pattern Adding :: IPRouteStatusMsg
pattern Adding = IPRouteStatusMsg' "Adding"

pattern RemoveFailed :: IPRouteStatusMsg
pattern RemoveFailed = IPRouteStatusMsg' "RemoveFailed"

pattern Removed :: IPRouteStatusMsg
pattern Removed = IPRouteStatusMsg' "Removed"

pattern Removing :: IPRouteStatusMsg
pattern Removing = IPRouteStatusMsg' "Removing"

{-# COMPLETE
  AddFailed,
  Added,
  Adding,
  RemoveFailed,
  Removed,
  Removing,
  IPRouteStatusMsg'
  #-}
