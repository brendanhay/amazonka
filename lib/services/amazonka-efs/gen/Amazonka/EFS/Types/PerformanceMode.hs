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
-- Module      : Amazonka.EFS.Types.PerformanceMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.PerformanceMode
  ( PerformanceMode
      ( ..,
        PerformanceMode_GeneralPurpose,
        PerformanceMode_MaxIO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PerformanceMode = PerformanceMode'
  { fromPerformanceMode ::
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

pattern PerformanceMode_GeneralPurpose :: PerformanceMode
pattern PerformanceMode_GeneralPurpose = PerformanceMode' "generalPurpose"

pattern PerformanceMode_MaxIO :: PerformanceMode
pattern PerformanceMode_MaxIO = PerformanceMode' "maxIO"

{-# COMPLETE
  PerformanceMode_GeneralPurpose,
  PerformanceMode_MaxIO,
  PerformanceMode'
  #-}
