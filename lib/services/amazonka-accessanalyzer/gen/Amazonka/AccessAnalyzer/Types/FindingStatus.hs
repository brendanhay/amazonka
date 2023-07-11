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
-- Module      : Amazonka.AccessAnalyzer.Types.FindingStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.FindingStatus
  ( FindingStatus
      ( ..,
        FindingStatus_ACTIVE,
        FindingStatus_ARCHIVED,
        FindingStatus_RESOLVED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FindingStatus = FindingStatus'
  { fromFindingStatus ::
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

pattern FindingStatus_ACTIVE :: FindingStatus
pattern FindingStatus_ACTIVE = FindingStatus' "ACTIVE"

pattern FindingStatus_ARCHIVED :: FindingStatus
pattern FindingStatus_ARCHIVED = FindingStatus' "ARCHIVED"

pattern FindingStatus_RESOLVED :: FindingStatus
pattern FindingStatus_RESOLVED = FindingStatus' "RESOLVED"

{-# COMPLETE
  FindingStatus_ACTIVE,
  FindingStatus_ARCHIVED,
  FindingStatus_RESOLVED,
  FindingStatus'
  #-}
