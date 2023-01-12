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
-- Module      : Amazonka.FSx.Types.DataRepositoryTaskType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryTaskType
  ( DataRepositoryTaskType
      ( ..,
        DataRepositoryTaskType_AUTO_RELEASE_DATA,
        DataRepositoryTaskType_EXPORT_TO_REPOSITORY,
        DataRepositoryTaskType_IMPORT_METADATA_FROM_REPOSITORY,
        DataRepositoryTaskType_RELEASE_DATA_FROM_FILESYSTEM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataRepositoryTaskType = DataRepositoryTaskType'
  { fromDataRepositoryTaskType ::
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

pattern DataRepositoryTaskType_AUTO_RELEASE_DATA :: DataRepositoryTaskType
pattern DataRepositoryTaskType_AUTO_RELEASE_DATA = DataRepositoryTaskType' "AUTO_RELEASE_DATA"

pattern DataRepositoryTaskType_EXPORT_TO_REPOSITORY :: DataRepositoryTaskType
pattern DataRepositoryTaskType_EXPORT_TO_REPOSITORY = DataRepositoryTaskType' "EXPORT_TO_REPOSITORY"

pattern DataRepositoryTaskType_IMPORT_METADATA_FROM_REPOSITORY :: DataRepositoryTaskType
pattern DataRepositoryTaskType_IMPORT_METADATA_FROM_REPOSITORY = DataRepositoryTaskType' "IMPORT_METADATA_FROM_REPOSITORY"

pattern DataRepositoryTaskType_RELEASE_DATA_FROM_FILESYSTEM :: DataRepositoryTaskType
pattern DataRepositoryTaskType_RELEASE_DATA_FROM_FILESYSTEM = DataRepositoryTaskType' "RELEASE_DATA_FROM_FILESYSTEM"

{-# COMPLETE
  DataRepositoryTaskType_AUTO_RELEASE_DATA,
  DataRepositoryTaskType_EXPORT_TO_REPOSITORY,
  DataRepositoryTaskType_IMPORT_METADATA_FROM_REPOSITORY,
  DataRepositoryTaskType_RELEASE_DATA_FROM_FILESYSTEM,
  DataRepositoryTaskType'
  #-}
