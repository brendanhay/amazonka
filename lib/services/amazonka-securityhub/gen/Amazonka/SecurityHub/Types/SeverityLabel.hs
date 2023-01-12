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
-- Module      : Amazonka.SecurityHub.Types.SeverityLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SeverityLabel
  ( SeverityLabel
      ( ..,
        SeverityLabel_CRITICAL,
        SeverityLabel_HIGH,
        SeverityLabel_INFORMATIONAL,
        SeverityLabel_LOW,
        SeverityLabel_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SeverityLabel = SeverityLabel'
  { fromSeverityLabel ::
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

pattern SeverityLabel_CRITICAL :: SeverityLabel
pattern SeverityLabel_CRITICAL = SeverityLabel' "CRITICAL"

pattern SeverityLabel_HIGH :: SeverityLabel
pattern SeverityLabel_HIGH = SeverityLabel' "HIGH"

pattern SeverityLabel_INFORMATIONAL :: SeverityLabel
pattern SeverityLabel_INFORMATIONAL = SeverityLabel' "INFORMATIONAL"

pattern SeverityLabel_LOW :: SeverityLabel
pattern SeverityLabel_LOW = SeverityLabel' "LOW"

pattern SeverityLabel_MEDIUM :: SeverityLabel
pattern SeverityLabel_MEDIUM = SeverityLabel' "MEDIUM"

{-# COMPLETE
  SeverityLabel_CRITICAL,
  SeverityLabel_HIGH,
  SeverityLabel_INFORMATIONAL,
  SeverityLabel_LOW,
  SeverityLabel_MEDIUM,
  SeverityLabel'
  #-}
