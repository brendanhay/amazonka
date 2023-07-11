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
-- Module      : Amazonka.CodeCommit.Types.FileModeTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.FileModeTypeEnum
  ( FileModeTypeEnum
      ( ..,
        FileModeTypeEnum_EXECUTABLE,
        FileModeTypeEnum_NORMAL,
        FileModeTypeEnum_SYMLINK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FileModeTypeEnum = FileModeTypeEnum'
  { fromFileModeTypeEnum ::
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

pattern FileModeTypeEnum_EXECUTABLE :: FileModeTypeEnum
pattern FileModeTypeEnum_EXECUTABLE = FileModeTypeEnum' "EXECUTABLE"

pattern FileModeTypeEnum_NORMAL :: FileModeTypeEnum
pattern FileModeTypeEnum_NORMAL = FileModeTypeEnum' "NORMAL"

pattern FileModeTypeEnum_SYMLINK :: FileModeTypeEnum
pattern FileModeTypeEnum_SYMLINK = FileModeTypeEnum' "SYMLINK"

{-# COMPLETE
  FileModeTypeEnum_EXECUTABLE,
  FileModeTypeEnum_NORMAL,
  FileModeTypeEnum_SYMLINK,
  FileModeTypeEnum'
  #-}
