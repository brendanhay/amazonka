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
        PatchPropertyProduct,
        PatchPropertyProductFamily,
        PatchPropertyClassification,
        PatchPropertyMsrcSeverity,
        PatchPropertyPriority,
        PatchPropertySeverity,
        fromPatchProperty
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PatchProperty = PatchProperty'
  { fromPatchProperty ::
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

pattern PatchPropertyProduct :: PatchProperty
pattern PatchPropertyProduct = PatchProperty' "PRODUCT"

pattern PatchPropertyProductFamily :: PatchProperty
pattern PatchPropertyProductFamily = PatchProperty' "PRODUCT_FAMILY"

pattern PatchPropertyClassification :: PatchProperty
pattern PatchPropertyClassification = PatchProperty' "CLASSIFICATION"

pattern PatchPropertyMsrcSeverity :: PatchProperty
pattern PatchPropertyMsrcSeverity = PatchProperty' "MSRC_SEVERITY"

pattern PatchPropertyPriority :: PatchProperty
pattern PatchPropertyPriority = PatchProperty' "PRIORITY"

pattern PatchPropertySeverity :: PatchProperty
pattern PatchPropertySeverity = PatchProperty' "SEVERITY"

{-# COMPLETE
  PatchPropertyProduct,
  PatchPropertyProductFamily,
  PatchPropertyClassification,
  PatchPropertyMsrcSeverity,
  PatchPropertyPriority,
  PatchPropertySeverity,
  PatchProperty'
  #-}
