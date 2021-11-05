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
-- Module      : Amazonka.MacieV2.Types.IsMonitoredByJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IsMonitoredByJob
  ( IsMonitoredByJob
      ( ..,
        IsMonitoredByJob_FALSE,
        IsMonitoredByJob_TRUE,
        IsMonitoredByJob_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype IsMonitoredByJob = IsMonitoredByJob'
  { fromIsMonitoredByJob ::
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

pattern IsMonitoredByJob_FALSE :: IsMonitoredByJob
pattern IsMonitoredByJob_FALSE = IsMonitoredByJob' "FALSE"

pattern IsMonitoredByJob_TRUE :: IsMonitoredByJob
pattern IsMonitoredByJob_TRUE = IsMonitoredByJob' "TRUE"

pattern IsMonitoredByJob_UNKNOWN :: IsMonitoredByJob
pattern IsMonitoredByJob_UNKNOWN = IsMonitoredByJob' "UNKNOWN"

{-# COMPLETE
  IsMonitoredByJob_FALSE,
  IsMonitoredByJob_TRUE,
  IsMonitoredByJob_UNKNOWN,
  IsMonitoredByJob'
  #-}
