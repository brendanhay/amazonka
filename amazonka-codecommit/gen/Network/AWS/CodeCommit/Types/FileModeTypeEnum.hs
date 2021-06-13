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
-- Module      : Network.AWS.CodeCommit.Types.FileModeTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileModeTypeEnum
  ( FileModeTypeEnum
      ( ..,
        FileModeTypeEnum_EXECUTABLE,
        FileModeTypeEnum_NORMAL,
        FileModeTypeEnum_SYMLINK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FileModeTypeEnum = FileModeTypeEnum'
  { fromFileModeTypeEnum ::
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
