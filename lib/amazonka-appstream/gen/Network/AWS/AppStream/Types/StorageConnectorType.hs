{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.StorageConnectorType
  ( StorageConnectorType
    ( StorageConnectorType'
    , StorageConnectorTypeHomefolders
    , StorageConnectorTypeGoogleDrive
    , StorageConnectorTypeOneDrive
    , fromStorageConnectorType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The type of storage connector.
newtype StorageConnectorType = StorageConnectorType'{fromStorageConnectorType
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern StorageConnectorTypeHomefolders :: StorageConnectorType
pattern StorageConnectorTypeHomefolders = StorageConnectorType' "HOMEFOLDERS"

pattern StorageConnectorTypeGoogleDrive :: StorageConnectorType
pattern StorageConnectorTypeGoogleDrive = StorageConnectorType' "GOOGLE_DRIVE"

pattern StorageConnectorTypeOneDrive :: StorageConnectorType
pattern StorageConnectorTypeOneDrive = StorageConnectorType' "ONE_DRIVE"

{-# COMPLETE 
  StorageConnectorTypeHomefolders,

  StorageConnectorTypeGoogleDrive,

  StorageConnectorTypeOneDrive,
  StorageConnectorType'
  #-}
