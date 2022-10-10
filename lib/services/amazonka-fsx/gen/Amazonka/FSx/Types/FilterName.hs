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
-- Module      : Amazonka.FSx.Types.FilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FilterName
  ( FilterName
      ( ..,
        FilterName_Backup_type,
        FilterName_Data_repository_type,
        FilterName_File_cache_id,
        FilterName_File_cache_type,
        FilterName_File_system_id,
        FilterName_File_system_type,
        FilterName_Volume_id
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The name for a filter.
newtype FilterName = FilterName'
  { fromFilterName ::
      Core.Text
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

pattern FilterName_Backup_type :: FilterName
pattern FilterName_Backup_type = FilterName' "backup-type"

pattern FilterName_Data_repository_type :: FilterName
pattern FilterName_Data_repository_type = FilterName' "data-repository-type"

pattern FilterName_File_cache_id :: FilterName
pattern FilterName_File_cache_id = FilterName' "file-cache-id"

pattern FilterName_File_cache_type :: FilterName
pattern FilterName_File_cache_type = FilterName' "file-cache-type"

pattern FilterName_File_system_id :: FilterName
pattern FilterName_File_system_id = FilterName' "file-system-id"

pattern FilterName_File_system_type :: FilterName
pattern FilterName_File_system_type = FilterName' "file-system-type"

pattern FilterName_Volume_id :: FilterName
pattern FilterName_Volume_id = FilterName' "volume-id"

{-# COMPLETE
  FilterName_Backup_type,
  FilterName_Data_repository_type,
  FilterName_File_cache_id,
  FilterName_File_cache_type,
  FilterName_File_system_id,
  FilterName_File_system_type,
  FilterName_Volume_id,
  FilterName'
  #-}
