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
-- Module      : Amazonka.LookoutVision.Types.ModelHostingStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelHostingStatus
  ( ModelHostingStatus
      ( ..,
        ModelHostingStatus_HOSTED,
        ModelHostingStatus_HOSTING_FAILED,
        ModelHostingStatus_STARTING_HOSTING,
        ModelHostingStatus_STOPPING_HOSTING,
        ModelHostingStatus_SYSTEM_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelHostingStatus = ModelHostingStatus'
  { fromModelHostingStatus ::
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

pattern ModelHostingStatus_HOSTED :: ModelHostingStatus
pattern ModelHostingStatus_HOSTED = ModelHostingStatus' "HOSTED"

pattern ModelHostingStatus_HOSTING_FAILED :: ModelHostingStatus
pattern ModelHostingStatus_HOSTING_FAILED = ModelHostingStatus' "HOSTING_FAILED"

pattern ModelHostingStatus_STARTING_HOSTING :: ModelHostingStatus
pattern ModelHostingStatus_STARTING_HOSTING = ModelHostingStatus' "STARTING_HOSTING"

pattern ModelHostingStatus_STOPPING_HOSTING :: ModelHostingStatus
pattern ModelHostingStatus_STOPPING_HOSTING = ModelHostingStatus' "STOPPING_HOSTING"

pattern ModelHostingStatus_SYSTEM_UPDATING :: ModelHostingStatus
pattern ModelHostingStatus_SYSTEM_UPDATING = ModelHostingStatus' "SYSTEM_UPDATING"

{-# COMPLETE
  ModelHostingStatus_HOSTED,
  ModelHostingStatus_HOSTING_FAILED,
  ModelHostingStatus_STARTING_HOSTING,
  ModelHostingStatus_STOPPING_HOSTING,
  ModelHostingStatus_SYSTEM_UPDATING,
  ModelHostingStatus'
  #-}
