{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DcFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DcFilter
  ( Eac3DcFilter
      ( Eac3DcFilter',
        Eac3DcFilterDisabled,
        Eac3DcFilterEnabled,
        fromEac3DcFilter
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Dc Filter
newtype Eac3DcFilter = Eac3DcFilter' {fromEac3DcFilter :: Core.Text}
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

pattern Eac3DcFilterDisabled :: Eac3DcFilter
pattern Eac3DcFilterDisabled = Eac3DcFilter' "DISABLED"

pattern Eac3DcFilterEnabled :: Eac3DcFilter
pattern Eac3DcFilterEnabled = Eac3DcFilter' "ENABLED"

{-# COMPLETE
  Eac3DcFilterDisabled,
  Eac3DcFilterEnabled,
  Eac3DcFilter'
  #-}
