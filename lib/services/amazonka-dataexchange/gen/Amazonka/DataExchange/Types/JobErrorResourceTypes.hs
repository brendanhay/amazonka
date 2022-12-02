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
-- Module      : Amazonka.DataExchange.Types.JobErrorResourceTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.JobErrorResourceTypes
  ( JobErrorResourceTypes
      ( ..,
        JobErrorResourceTypes_ASSET,
        JobErrorResourceTypes_DATA_SET,
        JobErrorResourceTypes_REVISION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobErrorResourceTypes = JobErrorResourceTypes'
  { fromJobErrorResourceTypes ::
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
