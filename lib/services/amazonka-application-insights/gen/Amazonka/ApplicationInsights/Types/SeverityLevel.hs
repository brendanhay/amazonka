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
-- Module      : Amazonka.ApplicationInsights.Types.SeverityLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.SeverityLevel
  ( SeverityLevel
      ( ..,
        SeverityLevel_High,
        SeverityLevel_Informative,
        SeverityLevel_Low,
        SeverityLevel_Medium
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SeverityLevel = SeverityLevel'
  { fromSeverityLevel ::
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

pattern SeverityLevel_High :: SeverityLevel
pattern SeverityLevel_High = SeverityLevel' "High"

pattern SeverityLevel_Informative :: SeverityLevel
pattern SeverityLevel_Informative = SeverityLevel' "Informative"

pattern SeverityLevel_Low :: SeverityLevel
pattern SeverityLevel_Low = SeverityLevel' "Low"

pattern SeverityLevel_Medium :: SeverityLevel
pattern SeverityLevel_Medium = SeverityLevel' "Medium"

{-# COMPLETE
  SeverityLevel_High,
  SeverityLevel_Informative,
  SeverityLevel_Low,
  SeverityLevel_Medium,
  SeverityLevel'
  #-}
