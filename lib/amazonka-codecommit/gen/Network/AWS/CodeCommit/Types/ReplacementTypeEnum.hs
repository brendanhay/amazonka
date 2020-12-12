{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReplacementTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReplacementTypeEnum
  ( ReplacementTypeEnum
      ( ReplacementTypeEnum',
        KeepBase,
        KeepDestination,
        KeepSource,
        UseNewContent
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReplacementTypeEnum = ReplacementTypeEnum' Lude.Text
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

pattern KeepBase :: ReplacementTypeEnum
pattern KeepBase = ReplacementTypeEnum' "KEEP_BASE"

pattern KeepDestination :: ReplacementTypeEnum
pattern KeepDestination = ReplacementTypeEnum' "KEEP_DESTINATION"

pattern KeepSource :: ReplacementTypeEnum
pattern KeepSource = ReplacementTypeEnum' "KEEP_SOURCE"

pattern UseNewContent :: ReplacementTypeEnum
pattern UseNewContent = ReplacementTypeEnum' "USE_NEW_CONTENT"

{-# COMPLETE
  KeepBase,
  KeepDestination,
  KeepSource,
  UseNewContent,
  ReplacementTypeEnum'
  #-}
