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
-- Module      : Amazonka.FSx.Types.DataRepositoryTaskFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryTaskFilterName
  ( DataRepositoryTaskFilterName
      ( ..,
        DataRepositoryTaskFilterName_Data_repository_association_id,
        DataRepositoryTaskFilterName_File_cache_id,
        DataRepositoryTaskFilterName_File_system_id,
        DataRepositoryTaskFilterName_Task_lifecycle
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataRepositoryTaskFilterName = DataRepositoryTaskFilterName'
  { fromDataRepositoryTaskFilterName ::
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

pattern DataRepositoryTaskFilterName_Data_repository_association_id :: DataRepositoryTaskFilterName
pattern DataRepositoryTaskFilterName_Data_repository_association_id = DataRepositoryTaskFilterName' "data-repository-association-id"

pattern DataRepositoryTaskFilterName_File_cache_id :: DataRepositoryTaskFilterName
pattern DataRepositoryTaskFilterName_File_cache_id = DataRepositoryTaskFilterName' "file-cache-id"

pattern DataRepositoryTaskFilterName_File_system_id :: DataRepositoryTaskFilterName
pattern DataRepositoryTaskFilterName_File_system_id = DataRepositoryTaskFilterName' "file-system-id"

pattern DataRepositoryTaskFilterName_Task_lifecycle :: DataRepositoryTaskFilterName
pattern DataRepositoryTaskFilterName_Task_lifecycle = DataRepositoryTaskFilterName' "task-lifecycle"

{-# COMPLETE
  DataRepositoryTaskFilterName_Data_repository_association_id,
  DataRepositoryTaskFilterName_File_cache_id,
  DataRepositoryTaskFilterName_File_system_id,
  DataRepositoryTaskFilterName_Task_lifecycle,
  DataRepositoryTaskFilterName'
  #-}
