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
-- Module      : Amazonka.AppStream.Types.StorageConnectorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.StorageConnectorType
  ( StorageConnectorType
      ( ..,
        StorageConnectorType_GOOGLE_DRIVE,
        StorageConnectorType_HOMEFOLDERS,
        StorageConnectorType_ONE_DRIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of storage connector.
newtype StorageConnectorType = StorageConnectorType'
  { fromStorageConnectorType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
