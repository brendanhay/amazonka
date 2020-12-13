{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StorageConnectorType
  ( StorageConnectorType
      ( StorageConnectorType',
        Homefolders,
        GoogleDrive,
        OneDrive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The type of storage connector.
newtype StorageConnectorType = StorageConnectorType' Lude.Text
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

pattern Homefolders :: StorageConnectorType
pattern Homefolders = StorageConnectorType' "HOMEFOLDERS"

pattern GoogleDrive :: StorageConnectorType
pattern GoogleDrive = StorageConnectorType' "GOOGLE_DRIVE"

pattern OneDrive :: StorageConnectorType
pattern OneDrive = StorageConnectorType' "ONE_DRIVE"

{-# COMPLETE
  Homefolders,
  GoogleDrive,
  OneDrive,
  StorageConnectorType'
  #-}
