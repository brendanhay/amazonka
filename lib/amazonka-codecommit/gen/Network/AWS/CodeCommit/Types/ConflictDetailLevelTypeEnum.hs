{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
  ( ConflictDetailLevelTypeEnum
      ( ConflictDetailLevelTypeEnum',
        FileLevel,
        LineLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConflictDetailLevelTypeEnum = ConflictDetailLevelTypeEnum' Lude.Text
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

pattern FileLevel :: ConflictDetailLevelTypeEnum
pattern FileLevel = ConflictDetailLevelTypeEnum' "FILE_LEVEL"

pattern LineLevel :: ConflictDetailLevelTypeEnum
pattern LineLevel = ConflictDetailLevelTypeEnum' "LINE_LEVEL"

{-# COMPLETE
  FileLevel,
  LineLevel,
  ConflictDetailLevelTypeEnum'
  #-}
