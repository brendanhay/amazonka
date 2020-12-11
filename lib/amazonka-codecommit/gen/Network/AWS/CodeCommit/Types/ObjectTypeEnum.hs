-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypeEnum
  ( ObjectTypeEnum
      ( ObjectTypeEnum',
        Directory,
        File,
        GitLink,
        SymbolicLink
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ObjectTypeEnum = ObjectTypeEnum' Lude.Text
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

pattern Directory :: ObjectTypeEnum
pattern Directory = ObjectTypeEnum' "DIRECTORY"

pattern File :: ObjectTypeEnum
pattern File = ObjectTypeEnum' "FILE"

pattern GitLink :: ObjectTypeEnum
pattern GitLink = ObjectTypeEnum' "GIT_LINK"

pattern SymbolicLink :: ObjectTypeEnum
pattern SymbolicLink = ObjectTypeEnum' "SYMBOLIC_LINK"

{-# COMPLETE
  Directory,
  File,
  GitLink,
  SymbolicLink,
  ObjectTypeEnum'
  #-}
