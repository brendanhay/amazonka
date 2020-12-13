{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerCatalogStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerCatalogStatus
  ( ServerCatalogStatus
      ( ServerCatalogStatus',
        SCSNotImported,
        SCSImporting,
        SCSAvailable,
        SCSDeleted,
        SCSExpired
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ServerCatalogStatus = ServerCatalogStatus' Lude.Text
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

pattern SCSNotImported :: ServerCatalogStatus
pattern SCSNotImported = ServerCatalogStatus' "NOT_IMPORTED"

pattern SCSImporting :: ServerCatalogStatus
pattern SCSImporting = ServerCatalogStatus' "IMPORTING"

pattern SCSAvailable :: ServerCatalogStatus
pattern SCSAvailable = ServerCatalogStatus' "AVAILABLE"

pattern SCSDeleted :: ServerCatalogStatus
pattern SCSDeleted = ServerCatalogStatus' "DELETED"

pattern SCSExpired :: ServerCatalogStatus
pattern SCSExpired = ServerCatalogStatus' "EXPIRED"

{-# COMPLETE
  SCSNotImported,
  SCSImporting,
  SCSAvailable,
  SCSDeleted,
  SCSExpired,
  ServerCatalogStatus'
  #-}
