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
-- Module      : Amazonka.SSM.Types.PatchProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchProperty
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PatchProperty = PatchProperty'
  { fromPatchProperty ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
