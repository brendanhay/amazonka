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
-- Module      : Amazonka.SageMaker.Types.ModelVariantStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelVariantStatus
  ( ModelVariantStatus
      ( ..,
        ModelVariantStatus_Creating,
        ModelVariantStatus_Deleted,
        ModelVariantStatus_Deleting,
        ModelVariantStatus_InService,
        ModelVariantStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelVariantStatus = ModelVariantStatus'
  { fromModelVariantStatus ::
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

pattern ModelVariantStatus_Creating :: ModelVariantStatus
pattern ModelVariantStatus_Creating = ModelVariantStatus' "Creating"

pattern ModelVariantStatus_Deleted :: ModelVariantStatus
pattern ModelVariantStatus_Deleted = ModelVariantStatus' "Deleted"

pattern ModelVariantStatus_Deleting :: ModelVariantStatus
pattern ModelVariantStatus_Deleting = ModelVariantStatus' "Deleting"

pattern ModelVariantStatus_InService :: ModelVariantStatus
pattern ModelVariantStatus_InService = ModelVariantStatus' "InService"

pattern ModelVariantStatus_Updating :: ModelVariantStatus
pattern ModelVariantStatus_Updating = ModelVariantStatus' "Updating"

{-# COMPLETE
  ModelVariantStatus_Creating,
  ModelVariantStatus_Deleted,
  ModelVariantStatus_Deleting,
  ModelVariantStatus_InService,
  ModelVariantStatus_Updating,
  ModelVariantStatus'
  #-}
