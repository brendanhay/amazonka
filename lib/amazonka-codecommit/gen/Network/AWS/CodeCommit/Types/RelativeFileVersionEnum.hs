{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
  ( RelativeFileVersionEnum
      ( RelativeFileVersionEnum',
        RelativeFileVersionEnumBefore,
        RelativeFileVersionEnumAfter,
        fromRelativeFileVersionEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RelativeFileVersionEnum = RelativeFileVersionEnum'
  { fromRelativeFileVersionEnum ::
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

pattern RelativeFileVersionEnumBefore :: RelativeFileVersionEnum
pattern RelativeFileVersionEnumBefore = RelativeFileVersionEnum' "BEFORE"

pattern RelativeFileVersionEnumAfter :: RelativeFileVersionEnum
pattern RelativeFileVersionEnumAfter = RelativeFileVersionEnum' "AFTER"

{-# COMPLETE
  RelativeFileVersionEnumBefore,
  RelativeFileVersionEnumAfter,
  RelativeFileVersionEnum'
  #-}
