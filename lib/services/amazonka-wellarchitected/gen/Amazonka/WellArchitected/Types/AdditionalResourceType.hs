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
-- Module      : Amazonka.WellArchitected.Types.AdditionalResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.AdditionalResourceType
  ( AdditionalResourceType
      ( ..,
        AdditionalResourceType_HELPFUL_RESOURCE,
        AdditionalResourceType_IMPROVEMENT_PLAN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AdditionalResourceType = AdditionalResourceType'
  { fromAdditionalResourceType ::
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

pattern AdditionalResourceType_HELPFUL_RESOURCE :: AdditionalResourceType
pattern AdditionalResourceType_HELPFUL_RESOURCE = AdditionalResourceType' "HELPFUL_RESOURCE"

pattern AdditionalResourceType_IMPROVEMENT_PLAN :: AdditionalResourceType
pattern AdditionalResourceType_IMPROVEMENT_PLAN = AdditionalResourceType' "IMPROVEMENT_PLAN"

{-# COMPLETE
  AdditionalResourceType_HELPFUL_RESOURCE,
  AdditionalResourceType_IMPROVEMENT_PLAN,
  AdditionalResourceType'
  #-}
