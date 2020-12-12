{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FlickerAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FlickerAq
  ( H265FlickerAq
      ( H265FlickerAq',
        HFADisabled,
        HFAEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Flicker Aq
newtype H265FlickerAq = H265FlickerAq' Lude.Text
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

pattern HFADisabled :: H265FlickerAq
pattern HFADisabled = H265FlickerAq' "DISABLED"

pattern HFAEnabled :: H265FlickerAq
pattern HFAEnabled = H265FlickerAq' "ENABLED"

{-# COMPLETE
  HFADisabled,
  HFAEnabled,
  H265FlickerAq'
  #-}
