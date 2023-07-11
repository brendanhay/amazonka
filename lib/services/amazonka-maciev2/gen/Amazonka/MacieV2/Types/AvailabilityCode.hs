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
-- Module      : Amazonka.MacieV2.Types.AvailabilityCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AvailabilityCode
  ( AvailabilityCode
      ( ..,
        AvailabilityCode_AVAILABLE,
        AvailabilityCode_UNAVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether occurrences of sensitive data can be retrieved for a
-- finding. Possible values are:
newtype AvailabilityCode = AvailabilityCode'
  { fromAvailabilityCode ::
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

pattern AvailabilityCode_AVAILABLE :: AvailabilityCode
pattern AvailabilityCode_AVAILABLE = AvailabilityCode' "AVAILABLE"

pattern AvailabilityCode_UNAVAILABLE :: AvailabilityCode
pattern AvailabilityCode_UNAVAILABLE = AvailabilityCode' "UNAVAILABLE"

{-# COMPLETE
  AvailabilityCode_AVAILABLE,
  AvailabilityCode_UNAVAILABLE,
  AvailabilityCode'
  #-}
