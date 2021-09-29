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
-- Module      : Network.AWS.Glue.Types.BlueprintStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BlueprintStatus
  ( BlueprintStatus
      ( ..,
        BlueprintStatus_ACTIVE,
        BlueprintStatus_CREATING,
        BlueprintStatus_FAILED,
        BlueprintStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BlueprintStatus = BlueprintStatus'
  { fromBlueprintStatus ::
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
