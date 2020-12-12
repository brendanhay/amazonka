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
        EDFDisabled,
        EDFEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Dc Filter
newtype Eac3DcFilter = Eac3DcFilter' Lude.Text
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

pattern EDFDisabled :: Eac3DcFilter
pattern EDFDisabled = Eac3DcFilter' "DISABLED"

pattern EDFEnabled :: Eac3DcFilter
pattern EDFEnabled = Eac3DcFilter' "ENABLED"

{-# COMPLETE
  EDFDisabled,
  EDFEnabled,
  Eac3DcFilter'
  #-}
