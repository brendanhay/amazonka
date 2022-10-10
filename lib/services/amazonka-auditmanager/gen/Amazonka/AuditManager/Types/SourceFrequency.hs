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
-- Module      : Amazonka.AuditManager.Types.SourceFrequency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.SourceFrequency
  ( SourceFrequency
      ( ..,
        SourceFrequency_DAILY,
        SourceFrequency_MONTHLY,
        SourceFrequency_WEEKLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SourceFrequency = SourceFrequency'
  { fromSourceFrequency ::
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

pattern SourceFrequency_DAILY :: SourceFrequency
pattern SourceFrequency_DAILY = SourceFrequency' "DAILY"

pattern SourceFrequency_MONTHLY :: SourceFrequency
pattern SourceFrequency_MONTHLY = SourceFrequency' "MONTHLY"

pattern SourceFrequency_WEEKLY :: SourceFrequency
pattern SourceFrequency_WEEKLY = SourceFrequency' "WEEKLY"

{-# COMPLETE
  SourceFrequency_DAILY,
  SourceFrequency_MONTHLY,
  SourceFrequency_WEEKLY,
  SourceFrequency'
  #-}
