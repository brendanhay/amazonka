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
-- Module      : Network.AWS.DataExchange.Types.JobErrorResourceTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types.JobErrorResourceTypes
  ( JobErrorResourceTypes
      ( ..,
        JobErrorResourceTypes_ASSET,
        JobErrorResourceTypes_DATA_SET,
        JobErrorResourceTypes_REVISION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The types of resource which the job error can apply to.
newtype JobErrorResourceTypes = JobErrorResourceTypes'
  { fromJobErrorResourceTypes ::
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

pattern JobErrorResourceTypes_ASSET :: JobErrorResourceTypes
pattern JobErrorResourceTypes_ASSET = JobErrorResourceTypes' "ASSET"

pattern JobErrorResourceTypes_DATA_SET :: JobErrorResourceTypes
pattern JobErrorResourceTypes_DATA_SET = JobErrorResourceTypes' "DATA_SET"

pattern JobErrorResourceTypes_REVISION :: JobErrorResourceTypes
pattern JobErrorResourceTypes_REVISION = JobErrorResourceTypes' "REVISION"

{-# COMPLETE
  JobErrorResourceTypes_ASSET,
  JobErrorResourceTypes_DATA_SET,
  JobErrorResourceTypes_REVISION,
  JobErrorResourceTypes'
  #-}
