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
-- Module      : Amazonka.ElastiCache.Types.ServiceUpdateSeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ServiceUpdateSeverity
  ( ServiceUpdateSeverity
      ( ..,
        ServiceUpdateSeverity_Critical,
        ServiceUpdateSeverity_Important,
        ServiceUpdateSeverity_Low,
        ServiceUpdateSeverity_Medium
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceUpdateSeverity = ServiceUpdateSeverity'
  { fromServiceUpdateSeverity ::
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

pattern ServiceUpdateSeverity_Critical :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Critical = ServiceUpdateSeverity' "critical"

pattern ServiceUpdateSeverity_Important :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Important = ServiceUpdateSeverity' "important"

pattern ServiceUpdateSeverity_Low :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Low = ServiceUpdateSeverity' "low"

pattern ServiceUpdateSeverity_Medium :: ServiceUpdateSeverity
pattern ServiceUpdateSeverity_Medium = ServiceUpdateSeverity' "medium"

{-# COMPLETE
  ServiceUpdateSeverity_Critical,
  ServiceUpdateSeverity_Important,
  ServiceUpdateSeverity_Low,
  ServiceUpdateSeverity_Medium,
  ServiceUpdateSeverity'
  #-}
