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
-- Module      : Amazonka.IoTSiteWise.Types.DetailedErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.DetailedErrorCode
  ( DetailedErrorCode
      ( ..,
        DetailedErrorCode_INCOMPATIBLE_COMPUTE_LOCATION,
        DetailedErrorCode_INCOMPATIBLE_FORWARDING_CONFIGURATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetailedErrorCode = DetailedErrorCode'
  { fromDetailedErrorCode ::
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

pattern DetailedErrorCode_INCOMPATIBLE_COMPUTE_LOCATION :: DetailedErrorCode
pattern DetailedErrorCode_INCOMPATIBLE_COMPUTE_LOCATION = DetailedErrorCode' "INCOMPATIBLE_COMPUTE_LOCATION"

pattern DetailedErrorCode_INCOMPATIBLE_FORWARDING_CONFIGURATION :: DetailedErrorCode
pattern DetailedErrorCode_INCOMPATIBLE_FORWARDING_CONFIGURATION = DetailedErrorCode' "INCOMPATIBLE_FORWARDING_CONFIGURATION"

{-# COMPLETE
  DetailedErrorCode_INCOMPATIBLE_COMPUTE_LOCATION,
  DetailedErrorCode_INCOMPATIBLE_FORWARDING_CONFIGURATION,
  DetailedErrorCode'
  #-}
