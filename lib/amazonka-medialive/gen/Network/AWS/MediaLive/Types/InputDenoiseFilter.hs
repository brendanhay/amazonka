{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDenoiseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDenoiseFilter
  ( InputDenoiseFilter
      ( InputDenoiseFilter',
        IDisabled,
        IEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Input Denoise Filter
newtype InputDenoiseFilter = InputDenoiseFilter' Lude.Text
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

pattern IDisabled :: InputDenoiseFilter
pattern IDisabled = InputDenoiseFilter' "DISABLED"

pattern IEnabled :: InputDenoiseFilter
pattern IEnabled = InputDenoiseFilter' "ENABLED"

{-# COMPLETE
  IDisabled,
  IEnabled,
  InputDenoiseFilter'
  #-}
