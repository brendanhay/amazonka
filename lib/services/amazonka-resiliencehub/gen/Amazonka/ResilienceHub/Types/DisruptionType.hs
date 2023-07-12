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
-- Module      : Amazonka.ResilienceHub.Types.DisruptionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.DisruptionType
  ( DisruptionType
      ( ..,
        DisruptionType_AZ,
        DisruptionType_Hardware,
        DisruptionType_Region,
        DisruptionType_Software
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DisruptionType = DisruptionType'
  { fromDisruptionType ::
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

pattern DisruptionType_AZ :: DisruptionType
pattern DisruptionType_AZ = DisruptionType' "AZ"

pattern DisruptionType_Hardware :: DisruptionType
pattern DisruptionType_Hardware = DisruptionType' "Hardware"

pattern DisruptionType_Region :: DisruptionType
pattern DisruptionType_Region = DisruptionType' "Region"

pattern DisruptionType_Software :: DisruptionType
pattern DisruptionType_Software = DisruptionType' "Software"

{-# COMPLETE
  DisruptionType_AZ,
  DisruptionType_Hardware,
  DisruptionType_Region,
  DisruptionType_Software,
  DisruptionType'
  #-}
