{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SourceType
  ( SourceType
      ( SourceType',
        Git,
        SVN,
        Archive,
        S3
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SourceType = SourceType' Lude.Text
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

pattern Git :: SourceType
pattern Git = SourceType' "git"

pattern SVN :: SourceType
pattern SVN = SourceType' "svn"

pattern Archive :: SourceType
pattern Archive = SourceType' "archive"

pattern S3 :: SourceType
pattern S3 = SourceType' "s3"

{-# COMPLETE
  Git,
  SVN,
  Archive,
  S3,
  SourceType'
  #-}
