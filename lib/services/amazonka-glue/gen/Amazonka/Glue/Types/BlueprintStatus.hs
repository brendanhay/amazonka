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
-- Module      : Amazonka.Glue.Types.BlueprintStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BlueprintStatus
  ( BlueprintStatus
      ( ..,
        BlueprintStatus_ACTIVE,
        BlueprintStatus_CREATING,
        BlueprintStatus_FAILED,
        BlueprintStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BlueprintStatus = BlueprintStatus'
  { fromBlueprintStatus ::
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

pattern BlueprintStatus_ACTIVE :: BlueprintStatus
pattern BlueprintStatus_ACTIVE = BlueprintStatus' "ACTIVE"

pattern BlueprintStatus_CREATING :: BlueprintStatus
pattern BlueprintStatus_CREATING = BlueprintStatus' "CREATING"

pattern BlueprintStatus_FAILED :: BlueprintStatus
pattern BlueprintStatus_FAILED = BlueprintStatus' "FAILED"

pattern BlueprintStatus_UPDATING :: BlueprintStatus
pattern BlueprintStatus_UPDATING = BlueprintStatus' "UPDATING"

{-# COMPLETE
  BlueprintStatus_ACTIVE,
  BlueprintStatus_CREATING,
  BlueprintStatus_FAILED,
  BlueprintStatus_UPDATING,
  BlueprintStatus'
  #-}
