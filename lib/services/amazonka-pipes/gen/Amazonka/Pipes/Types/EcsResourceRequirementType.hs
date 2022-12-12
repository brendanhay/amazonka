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
-- Module      : Amazonka.Pipes.Types.EcsResourceRequirementType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsResourceRequirementType
  ( EcsResourceRequirementType
      ( ..,
        EcsResourceRequirementType_GPU,
        EcsResourceRequirementType_InferenceAccelerator
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EcsResourceRequirementType = EcsResourceRequirementType'
  { fromEcsResourceRequirementType ::
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

pattern EcsResourceRequirementType_GPU :: EcsResourceRequirementType
pattern EcsResourceRequirementType_GPU = EcsResourceRequirementType' "GPU"

pattern EcsResourceRequirementType_InferenceAccelerator :: EcsResourceRequirementType
pattern EcsResourceRequirementType_InferenceAccelerator = EcsResourceRequirementType' "InferenceAccelerator"

{-# COMPLETE
  EcsResourceRequirementType_GPU,
  EcsResourceRequirementType_InferenceAccelerator,
  EcsResourceRequirementType'
  #-}
