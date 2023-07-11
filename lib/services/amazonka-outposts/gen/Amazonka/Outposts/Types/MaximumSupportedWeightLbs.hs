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
-- Module      : Amazonka.Outposts.Types.MaximumSupportedWeightLbs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.MaximumSupportedWeightLbs
  ( MaximumSupportedWeightLbs
      ( ..,
        MaximumSupportedWeightLbs_MAX_1400_LBS,
        MaximumSupportedWeightLbs_MAX_1600_LBS,
        MaximumSupportedWeightLbs_MAX_1800_LBS,
        MaximumSupportedWeightLbs_MAX_2000_LBS,
        MaximumSupportedWeightLbs_NO_LIMIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MaximumSupportedWeightLbs = MaximumSupportedWeightLbs'
  { fromMaximumSupportedWeightLbs ::
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

pattern MaximumSupportedWeightLbs_MAX_1400_LBS :: MaximumSupportedWeightLbs
pattern MaximumSupportedWeightLbs_MAX_1400_LBS = MaximumSupportedWeightLbs' "MAX_1400_LBS"

pattern MaximumSupportedWeightLbs_MAX_1600_LBS :: MaximumSupportedWeightLbs
pattern MaximumSupportedWeightLbs_MAX_1600_LBS = MaximumSupportedWeightLbs' "MAX_1600_LBS"

pattern MaximumSupportedWeightLbs_MAX_1800_LBS :: MaximumSupportedWeightLbs
pattern MaximumSupportedWeightLbs_MAX_1800_LBS = MaximumSupportedWeightLbs' "MAX_1800_LBS"

pattern MaximumSupportedWeightLbs_MAX_2000_LBS :: MaximumSupportedWeightLbs
pattern MaximumSupportedWeightLbs_MAX_2000_LBS = MaximumSupportedWeightLbs' "MAX_2000_LBS"

pattern MaximumSupportedWeightLbs_NO_LIMIT :: MaximumSupportedWeightLbs
pattern MaximumSupportedWeightLbs_NO_LIMIT = MaximumSupportedWeightLbs' "NO_LIMIT"

{-# COMPLETE
  MaximumSupportedWeightLbs_MAX_1400_LBS,
  MaximumSupportedWeightLbs_MAX_1600_LBS,
  MaximumSupportedWeightLbs_MAX_1800_LBS,
  MaximumSupportedWeightLbs_MAX_2000_LBS,
  MaximumSupportedWeightLbs_NO_LIMIT,
  MaximumSupportedWeightLbs'
  #-}
