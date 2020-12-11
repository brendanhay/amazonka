-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileModeTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileModeTypeEnum
  ( FileModeTypeEnum
      ( FileModeTypeEnum',
        Executable,
        Normal,
        Symlink
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FileModeTypeEnum = FileModeTypeEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Executable :: FileModeTypeEnum
pattern Executable = FileModeTypeEnum' "EXECUTABLE"

pattern Normal :: FileModeTypeEnum
pattern Normal = FileModeTypeEnum' "NORMAL"

pattern Symlink :: FileModeTypeEnum
pattern Symlink = FileModeTypeEnum' "SYMLINK"

{-# COMPLETE
  Executable,
  Normal,
  Symlink,
  FileModeTypeEnum'
  #-}
