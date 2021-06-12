{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnectorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StorageConnectorType
  ( StorageConnectorType
      ( ..,
        StorageConnectorType_GOOGLE_DRIVE,
        StorageConnectorType_HOMEFOLDERS,
        StorageConnectorType_ONE_DRIVE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The type of storage connector.
newtype StorageConnectorType = StorageConnectorType'
  { fromStorageConnectorType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern StorageConnectorType_GOOGLE_DRIVE :: StorageConnectorType
pattern StorageConnectorType_GOOGLE_DRIVE = StorageConnectorType' "GOOGLE_DRIVE"

pattern StorageConnectorType_HOMEFOLDERS :: StorageConnectorType
pattern StorageConnectorType_HOMEFOLDERS = StorageConnectorType' "HOMEFOLDERS"

pattern StorageConnectorType_ONE_DRIVE :: StorageConnectorType
pattern StorageConnectorType_ONE_DRIVE = StorageConnectorType' "ONE_DRIVE"

{-# COMPLETE
  StorageConnectorType_GOOGLE_DRIVE,
  StorageConnectorType_HOMEFOLDERS,
  StorageConnectorType_ONE_DRIVE,
  StorageConnectorType'
  #-}
