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
-- Module      : Amazonka.Pipes.Types.BatchResourceRequirementType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.BatchResourceRequirementType
  ( BatchResourceRequirementType
      ( ..,
        BatchResourceRequirementType_GPU,
        BatchResourceRequirementType_MEMORY,
        BatchResourceRequirementType_VCPU
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchResourceRequirementType = BatchResourceRequirementType'
  { fromBatchResourceRequirementType ::
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

pattern BatchResourceRequirementType_GPU :: BatchResourceRequirementType
pattern BatchResourceRequirementType_GPU = BatchResourceRequirementType' "GPU"

pattern BatchResourceRequirementType_MEMORY :: BatchResourceRequirementType
pattern BatchResourceRequirementType_MEMORY = BatchResourceRequirementType' "MEMORY"

pattern BatchResourceRequirementType_VCPU :: BatchResourceRequirementType
pattern BatchResourceRequirementType_VCPU = BatchResourceRequirementType' "VCPU"

{-# COMPLETE
  BatchResourceRequirementType_GPU,
  BatchResourceRequirementType_MEMORY,
  BatchResourceRequirementType_VCPU,
  BatchResourceRequirementType'
  #-}
