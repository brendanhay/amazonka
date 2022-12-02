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
-- Module      : Amazonka.FSx.Types.FileSystemLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileSystemLifecycle
  ( FileSystemLifecycle
      ( ..,
        FileSystemLifecycle_AVAILABLE,
        FileSystemLifecycle_CREATING,
        FileSystemLifecycle_DELETING,
        FileSystemLifecycle_FAILED,
        FileSystemLifecycle_MISCONFIGURED,
        FileSystemLifecycle_MISCONFIGURED_UNAVAILABLE,
        FileSystemLifecycle_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The lifecycle status of the file system.
newtype FileSystemLifecycle = FileSystemLifecycle'
  { fromFileSystemLifecycle ::
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

pattern FileSystemLifecycle_AVAILABLE :: FileSystemLifecycle
pattern FileSystemLifecycle_AVAILABLE = FileSystemLifecycle' "AVAILABLE"

pattern FileSystemLifecycle_CREATING :: FileSystemLifecycle
pattern FileSystemLifecycle_CREATING = FileSystemLifecycle' "CREATING"

pattern FileSystemLifecycle_DELETING :: FileSystemLifecycle
pattern FileSystemLifecycle_DELETING = FileSystemLifecycle' "DELETING"

pattern FileSystemLifecycle_FAILED :: FileSystemLifecycle
pattern FileSystemLifecycle_FAILED = FileSystemLifecycle' "FAILED"

pattern FileSystemLifecycle_MISCONFIGURED :: FileSystemLifecycle
pattern FileSystemLifecycle_MISCONFIGURED = FileSystemLifecycle' "MISCONFIGURED"

pattern FileSystemLifecycle_MISCONFIGURED_UNAVAILABLE :: FileSystemLifecycle
pattern FileSystemLifecycle_MISCONFIGURED_UNAVAILABLE = FileSystemLifecycle' "MISCONFIGURED_UNAVAILABLE"

pattern FileSystemLifecycle_UPDATING :: FileSystemLifecycle
pattern FileSystemLifecycle_UPDATING = FileSystemLifecycle' "UPDATING"

{-# COMPLETE
  FileSystemLifecycle_AVAILABLE,
  FileSystemLifecycle_CREATING,
  FileSystemLifecycle_DELETING,
  FileSystemLifecycle_FAILED,
  FileSystemLifecycle_MISCONFIGURED,
  FileSystemLifecycle_MISCONFIGURED_UNAVAILABLE,
  FileSystemLifecycle_UPDATING,
  FileSystemLifecycle'
  #-}
