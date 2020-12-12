{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchProperty
  ( PatchProperty
      ( PatchProperty',
        PPClassification,
        PPMsrcSeverity,
        PPPriority,
        PPProduct,
        PPProductFamily,
        PPSeverity
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PatchProperty = PatchProperty' Lude.Text
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

pattern PPClassification :: PatchProperty
pattern PPClassification = PatchProperty' "CLASSIFICATION"

pattern PPMsrcSeverity :: PatchProperty
pattern PPMsrcSeverity = PatchProperty' "MSRC_SEVERITY"

pattern PPPriority :: PatchProperty
pattern PPPriority = PatchProperty' "PRIORITY"

pattern PPProduct :: PatchProperty
pattern PPProduct = PatchProperty' "PRODUCT"

pattern PPProductFamily :: PatchProperty
pattern PPProductFamily = PatchProperty' "PRODUCT_FAMILY"

pattern PPSeverity :: PatchProperty
pattern PPSeverity = PatchProperty' "SEVERITY"

{-# COMPLETE
  PPClassification,
  PPMsrcSeverity,
  PPPriority,
  PPProduct,
  PPProductFamily,
  PPSeverity,
  PatchProperty'
  #-}
