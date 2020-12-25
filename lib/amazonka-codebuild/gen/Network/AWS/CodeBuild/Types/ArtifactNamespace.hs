{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ArtifactNamespace
  ( ArtifactNamespace
      ( ArtifactNamespace',
        ArtifactNamespaceNone,
        ArtifactNamespaceBuildId,
        fromArtifactNamespace
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ArtifactNamespace = ArtifactNamespace'
  { fromArtifactNamespace ::
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

pattern ArtifactNamespaceNone :: ArtifactNamespace
pattern ArtifactNamespaceNone = ArtifactNamespace' "NONE"

pattern ArtifactNamespaceBuildId :: ArtifactNamespace
pattern ArtifactNamespaceBuildId = ArtifactNamespace' "BUILD_ID"

{-# COMPLETE
  ArtifactNamespaceNone,
  ArtifactNamespaceBuildId,
  ArtifactNamespace'
  #-}
