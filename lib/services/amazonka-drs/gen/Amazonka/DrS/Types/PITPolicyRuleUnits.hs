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
-- Module      : Amazonka.DrS.Types.PITPolicyRuleUnits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.PITPolicyRuleUnits
  ( PITPolicyRuleUnits
      ( ..,
        PITPolicyRuleUnits_DAY,
        PITPolicyRuleUnits_HOUR,
        PITPolicyRuleUnits_MINUTE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PITPolicyRuleUnits = PITPolicyRuleUnits'
  { fromPITPolicyRuleUnits ::
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

pattern PITPolicyRuleUnits_DAY :: PITPolicyRuleUnits
pattern PITPolicyRuleUnits_DAY = PITPolicyRuleUnits' "DAY"

pattern PITPolicyRuleUnits_HOUR :: PITPolicyRuleUnits
pattern PITPolicyRuleUnits_HOUR = PITPolicyRuleUnits' "HOUR"

pattern PITPolicyRuleUnits_MINUTE :: PITPolicyRuleUnits
pattern PITPolicyRuleUnits_MINUTE = PITPolicyRuleUnits' "MINUTE"

{-# COMPLETE
  PITPolicyRuleUnits_DAY,
  PITPolicyRuleUnits_HOUR,
  PITPolicyRuleUnits_MINUTE,
  PITPolicyRuleUnits'
  #-}
