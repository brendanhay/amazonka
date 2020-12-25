{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ObjectTypeEnumFile,
        ObjectTypeEnumDirectory,
        ObjectTypeEnumGitLink,
        ObjectTypeEnumSymbolicLink,
        fromObjectTypeEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ObjectTypeEnum = ObjectTypeEnum'
  { fromObjectTypeEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ObjectTypeEnumFile :: ObjectTypeEnum
pattern ObjectTypeEnumFile = ObjectTypeEnum' "FILE"

pattern ObjectTypeEnumDirectory :: ObjectTypeEnum
pattern ObjectTypeEnumDirectory = ObjectTypeEnum' "DIRECTORY"

pattern ObjectTypeEnumGitLink :: ObjectTypeEnum
pattern ObjectTypeEnumGitLink = ObjectTypeEnum' "GIT_LINK"

pattern ObjectTypeEnumSymbolicLink :: ObjectTypeEnum
pattern ObjectTypeEnumSymbolicLink = ObjectTypeEnum' "SYMBOLIC_LINK"

{-# COMPLETE
  ObjectTypeEnumFile,
  ObjectTypeEnumDirectory,
  ObjectTypeEnumGitLink,
  ObjectTypeEnumSymbolicLink,
  ObjectTypeEnum'
  #-}
