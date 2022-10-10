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
-- Module      : Amazonka.SecurityHub.Types.SeverityRating
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SeverityRating
  ( SeverityRating
      ( ..,
        SeverityRating_CRITICAL,
        SeverityRating_HIGH,
        SeverityRating_LOW,
        SeverityRating_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SeverityRating = SeverityRating'
  { fromSeverityRating ::
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

pattern SeverityRating_CRITICAL :: SeverityRating
pattern SeverityRating_CRITICAL = SeverityRating' "CRITICAL"

pattern SeverityRating_HIGH :: SeverityRating
pattern SeverityRating_HIGH = SeverityRating' "HIGH"

pattern SeverityRating_LOW :: SeverityRating
pattern SeverityRating_LOW = SeverityRating' "LOW"

pattern SeverityRating_MEDIUM :: SeverityRating
pattern SeverityRating_MEDIUM = SeverityRating' "MEDIUM"

{-# COMPLETE
  SeverityRating_CRITICAL,
  SeverityRating_HIGH,
  SeverityRating_LOW,
  SeverityRating_MEDIUM,
  SeverityRating'
  #-}
