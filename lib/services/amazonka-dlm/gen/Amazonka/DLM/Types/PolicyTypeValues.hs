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
-- Module      : Amazonka.DLM.Types.PolicyTypeValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.PolicyTypeValues
  ( PolicyTypeValues
      ( ..,
        PolicyTypeValues_EBS_SNAPSHOT_MANAGEMENT,
        PolicyTypeValues_EVENT_BASED_POLICY,
        PolicyTypeValues_IMAGE_MANAGEMENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PolicyTypeValues = PolicyTypeValues'
  { fromPolicyTypeValues ::
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

pattern PolicyTypeValues_EBS_SNAPSHOT_MANAGEMENT :: PolicyTypeValues
pattern PolicyTypeValues_EBS_SNAPSHOT_MANAGEMENT = PolicyTypeValues' "EBS_SNAPSHOT_MANAGEMENT"

pattern PolicyTypeValues_EVENT_BASED_POLICY :: PolicyTypeValues
pattern PolicyTypeValues_EVENT_BASED_POLICY = PolicyTypeValues' "EVENT_BASED_POLICY"

pattern PolicyTypeValues_IMAGE_MANAGEMENT :: PolicyTypeValues
pattern PolicyTypeValues_IMAGE_MANAGEMENT = PolicyTypeValues' "IMAGE_MANAGEMENT"

{-# COMPLETE
  PolicyTypeValues_EBS_SNAPSHOT_MANAGEMENT,
  PolicyTypeValues_EVENT_BASED_POLICY,
  PolicyTypeValues_IMAGE_MANAGEMENT,
  PolicyTypeValues'
  #-}
