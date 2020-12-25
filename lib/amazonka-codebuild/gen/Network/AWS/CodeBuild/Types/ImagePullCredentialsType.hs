{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ImagePullCredentialsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ImagePullCredentialsType
  ( ImagePullCredentialsType
      ( ImagePullCredentialsType',
        ImagePullCredentialsTypeCodebuild,
        ImagePullCredentialsTypeServiceRole,
        fromImagePullCredentialsType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ImagePullCredentialsType = ImagePullCredentialsType'
  { fromImagePullCredentialsType ::
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

pattern ImagePullCredentialsTypeCodebuild :: ImagePullCredentialsType
pattern ImagePullCredentialsTypeCodebuild = ImagePullCredentialsType' "CODEBUILD"

pattern ImagePullCredentialsTypeServiceRole :: ImagePullCredentialsType
pattern ImagePullCredentialsTypeServiceRole = ImagePullCredentialsType' "SERVICE_ROLE"

{-# COMPLETE
  ImagePullCredentialsTypeCodebuild,
  ImagePullCredentialsTypeServiceRole,
  ImagePullCredentialsType'
  #-}
