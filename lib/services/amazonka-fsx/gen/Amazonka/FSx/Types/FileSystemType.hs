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
-- Module      : Amazonka.FSx.Types.FileSystemType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileSystemType
  ( FileSystemType
      ( ..,
        FileSystemType_LUSTRE,
        FileSystemType_ONTAP,
        FileSystemType_OPENZFS,
        FileSystemType_WINDOWS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The type of file system.
newtype FileSystemType = FileSystemType'
  { fromFileSystemType ::
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

pattern FileSystemType_LUSTRE :: FileSystemType
pattern FileSystemType_LUSTRE = FileSystemType' "LUSTRE"

pattern FileSystemType_ONTAP :: FileSystemType
pattern FileSystemType_ONTAP = FileSystemType' "ONTAP"

pattern FileSystemType_OPENZFS :: FileSystemType
pattern FileSystemType_OPENZFS = FileSystemType' "OPENZFS"

pattern FileSystemType_WINDOWS :: FileSystemType
pattern FileSystemType_WINDOWS = FileSystemType' "WINDOWS"

{-# COMPLETE
  FileSystemType_LUSTRE,
  FileSystemType_ONTAP,
  FileSystemType_OPENZFS,
  FileSystemType_WINDOWS,
  FileSystemType'
  #-}
