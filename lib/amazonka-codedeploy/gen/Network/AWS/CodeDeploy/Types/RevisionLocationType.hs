{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.RevisionLocationType
  ( RevisionLocationType
    ( RevisionLocationType'
    , RevisionLocationTypeS3
    , RevisionLocationTypeGitHub
    , RevisionLocationTypeString
    , RevisionLocationTypeAppSpecContent
    , fromRevisionLocationType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RevisionLocationType = RevisionLocationType'{fromRevisionLocationType
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern RevisionLocationTypeS3 :: RevisionLocationType
pattern RevisionLocationTypeS3 = RevisionLocationType' "S3"

pattern RevisionLocationTypeGitHub :: RevisionLocationType
pattern RevisionLocationTypeGitHub = RevisionLocationType' "GitHub"

pattern RevisionLocationTypeString :: RevisionLocationType
pattern RevisionLocationTypeString = RevisionLocationType' "String"

pattern RevisionLocationTypeAppSpecContent :: RevisionLocationType
pattern RevisionLocationTypeAppSpecContent = RevisionLocationType' "AppSpecContent"

{-# COMPLETE 
  RevisionLocationTypeS3,

  RevisionLocationTypeGitHub,

  RevisionLocationTypeString,

  RevisionLocationTypeAppSpecContent,
  RevisionLocationType'
  #-}
