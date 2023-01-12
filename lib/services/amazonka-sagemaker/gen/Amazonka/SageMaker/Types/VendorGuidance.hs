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
-- Module      : Amazonka.SageMaker.Types.VendorGuidance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.VendorGuidance
  ( VendorGuidance
      ( ..,
        VendorGuidance_ARCHIVED,
        VendorGuidance_NOT_PROVIDED,
        VendorGuidance_STABLE,
        VendorGuidance_TO_BE_ARCHIVED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VendorGuidance = VendorGuidance'
  { fromVendorGuidance ::
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

pattern VendorGuidance_ARCHIVED :: VendorGuidance
pattern VendorGuidance_ARCHIVED = VendorGuidance' "ARCHIVED"

pattern VendorGuidance_NOT_PROVIDED :: VendorGuidance
pattern VendorGuidance_NOT_PROVIDED = VendorGuidance' "NOT_PROVIDED"

pattern VendorGuidance_STABLE :: VendorGuidance
pattern VendorGuidance_STABLE = VendorGuidance' "STABLE"

pattern VendorGuidance_TO_BE_ARCHIVED :: VendorGuidance
pattern VendorGuidance_TO_BE_ARCHIVED = VendorGuidance' "TO_BE_ARCHIVED"

{-# COMPLETE
  VendorGuidance_ARCHIVED,
  VendorGuidance_NOT_PROVIDED,
  VendorGuidance_STABLE,
  VendorGuidance_TO_BE_ARCHIVED,
  VendorGuidance'
  #-}
