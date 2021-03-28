{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.SourceType
  ( SourceType
    ( SourceType'
    , SourceTypeGit
    , SourceTypeSvn
    , SourceTypeArchive
    , SourceTypeS3
    , fromSourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SourceType = SourceType'{fromSourceType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern SourceTypeGit :: SourceType
pattern SourceTypeGit = SourceType' "git"

pattern SourceTypeSvn :: SourceType
pattern SourceTypeSvn = SourceType' "svn"

pattern SourceTypeArchive :: SourceType
pattern SourceTypeArchive = SourceType' "archive"

pattern SourceTypeS3 :: SourceType
pattern SourceTypeS3 = SourceType' "s3"

{-# COMPLETE 
  SourceTypeGit,

  SourceTypeSvn,

  SourceTypeArchive,

  SourceTypeS3,
  SourceType'
  #-}
