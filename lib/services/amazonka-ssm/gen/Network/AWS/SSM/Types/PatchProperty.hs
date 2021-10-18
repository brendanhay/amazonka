{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchProperty
  ( PatchProperty
      ( ..,
        PatchProperty_CLASSIFICATION,
        PatchProperty_MSRC_SEVERITY,
        PatchProperty_PRIORITY,
        PatchProperty_PRODUCT,
        PatchProperty_PRODUCT_FAMILY,
        PatchProperty_SEVERITY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PatchProperty = PatchProperty'
  { fromPatchProperty ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PatchProperty_CLASSIFICATION :: PatchProperty
pattern PatchProperty_CLASSIFICATION = PatchProperty' "CLASSIFICATION"

pattern PatchProperty_MSRC_SEVERITY :: PatchProperty
pattern PatchProperty_MSRC_SEVERITY = PatchProperty' "MSRC_SEVERITY"

pattern PatchProperty_PRIORITY :: PatchProperty
pattern PatchProperty_PRIORITY = PatchProperty' "PRIORITY"

pattern PatchProperty_PRODUCT :: PatchProperty
pattern PatchProperty_PRODUCT = PatchProperty' "PRODUCT"

pattern PatchProperty_PRODUCT_FAMILY :: PatchProperty
pattern PatchProperty_PRODUCT_FAMILY = PatchProperty' "PRODUCT_FAMILY"

pattern PatchProperty_SEVERITY :: PatchProperty
pattern PatchProperty_SEVERITY = PatchProperty' "SEVERITY"

{-# COMPLETE
  PatchProperty_CLASSIFICATION,
  PatchProperty_MSRC_SEVERITY,
  PatchProperty_PRIORITY,
  PatchProperty_PRODUCT,
  PatchProperty_PRODUCT_FAMILY,
  PatchProperty_SEVERITY,
  PatchProperty'
  #-}
