-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ArtifactsType
  ( ArtifactsType
      ( ArtifactsType',
        Codepipeline,
        NoArtifacts,
        S3
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ArtifactsType = ArtifactsType' Lude.Text
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

pattern Codepipeline :: ArtifactsType
pattern Codepipeline = ArtifactsType' "CODEPIPELINE"

pattern NoArtifacts :: ArtifactsType
pattern NoArtifacts = ArtifactsType' "NO_ARTIFACTS"

pattern S3 :: ArtifactsType
pattern S3 = ArtifactsType' "S3"

{-# COMPLETE
  Codepipeline,
  NoArtifacts,
  S3,
  ArtifactsType'
  #-}
