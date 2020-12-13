{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ATCodepipeline,
        ATS3,
        ATNoArtifacts
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

pattern ATCodepipeline :: ArtifactsType
pattern ATCodepipeline = ArtifactsType' "CODEPIPELINE"

pattern ATS3 :: ArtifactsType
pattern ATS3 = ArtifactsType' "S3"

pattern ATNoArtifacts :: ArtifactsType
pattern ATNoArtifacts = ArtifactsType' "NO_ARTIFACTS"

{-# COMPLETE
  ATCodepipeline,
  ATS3,
  ATNoArtifacts,
  ArtifactsType'
  #-}
