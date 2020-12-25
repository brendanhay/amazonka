{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Profile
  ( H265Profile
      ( H265Profile',
        H265ProfileMain,
        H265ProfileMain10BIT,
        fromH265Profile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H265 Profile
newtype H265Profile = H265Profile' {fromH265Profile :: Core.Text}
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

pattern H265ProfileMain :: H265Profile
pattern H265ProfileMain = H265Profile' "MAIN"

pattern H265ProfileMain10BIT :: H265Profile
pattern H265ProfileMain10BIT = H265Profile' "MAIN_10BIT"

{-# COMPLETE
  H265ProfileMain,
  H265ProfileMain10BIT,
  H265Profile'
  #-}
