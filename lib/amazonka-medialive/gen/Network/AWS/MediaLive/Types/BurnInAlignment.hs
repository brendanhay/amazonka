{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInAlignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInAlignment
  ( BurnInAlignment
      ( BurnInAlignment',
        BurnInAlignmentCentered,
        BurnInAlignmentLeft,
        BurnInAlignmentSmart,
        fromBurnInAlignment
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Burn In Alignment
newtype BurnInAlignment = BurnInAlignment'
  { fromBurnInAlignment ::
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

pattern BurnInAlignmentCentered :: BurnInAlignment
pattern BurnInAlignmentCentered = BurnInAlignment' "CENTERED"

pattern BurnInAlignmentLeft :: BurnInAlignment
pattern BurnInAlignmentLeft = BurnInAlignment' "LEFT"

pattern BurnInAlignmentSmart :: BurnInAlignment
pattern BurnInAlignmentSmart = BurnInAlignment' "SMART"

{-# COMPLETE
  BurnInAlignmentCentered,
  BurnInAlignmentLeft,
  BurnInAlignmentSmart,
  BurnInAlignment'
  #-}
