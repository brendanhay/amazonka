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
-- Module      : Amazonka.MacieV2.Types.JobComparator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.JobComparator
  ( JobComparator
      ( ..,
        JobComparator_CONTAINS,
        JobComparator_EQ,
        JobComparator_GT,
        JobComparator_GTE,
        JobComparator_LT,
        JobComparator_LTE,
        JobComparator_NE,
        JobComparator_STARTS_WITH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The operator to use in a condition. Depending on the type of condition,
-- possible values are:
newtype JobComparator = JobComparator'
  { fromJobComparator ::
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

pattern JobComparator_CONTAINS :: JobComparator
pattern JobComparator_CONTAINS = JobComparator' "CONTAINS"

pattern JobComparator_EQ :: JobComparator
pattern JobComparator_EQ = JobComparator' "EQ"

pattern JobComparator_GT :: JobComparator
pattern JobComparator_GT = JobComparator' "GT"

pattern JobComparator_GTE :: JobComparator
pattern JobComparator_GTE = JobComparator' "GTE"

pattern JobComparator_LT :: JobComparator
pattern JobComparator_LT = JobComparator' "LT"

pattern JobComparator_LTE :: JobComparator
pattern JobComparator_LTE = JobComparator' "LTE"

pattern JobComparator_NE :: JobComparator
pattern JobComparator_NE = JobComparator' "NE"

pattern JobComparator_STARTS_WITH :: JobComparator
pattern JobComparator_STARTS_WITH = JobComparator' "STARTS_WITH"

{-# COMPLETE
  JobComparator_CONTAINS,
  JobComparator_EQ,
  JobComparator_GT,
  JobComparator_GTE,
  JobComparator_LT,
  JobComparator_LTE,
  JobComparator_NE,
  JobComparator_STARTS_WITH,
  JobComparator'
  #-}
