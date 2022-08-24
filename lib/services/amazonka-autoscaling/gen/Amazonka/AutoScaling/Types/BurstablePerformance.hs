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
-- Module      : Amazonka.AutoScaling.Types.BurstablePerformance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.BurstablePerformance
  ( BurstablePerformance
      ( ..,
        BurstablePerformance_Excluded,
        BurstablePerformance_Included,
        BurstablePerformance_Required
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype BurstablePerformance = BurstablePerformance'
  { fromBurstablePerformance ::
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

pattern BurstablePerformance_Excluded :: BurstablePerformance
pattern BurstablePerformance_Excluded = BurstablePerformance' "excluded"

pattern BurstablePerformance_Included :: BurstablePerformance
pattern BurstablePerformance_Included = BurstablePerformance' "included"

pattern BurstablePerformance_Required :: BurstablePerformance
pattern BurstablePerformance_Required = BurstablePerformance' "required"

{-# COMPLETE
  BurstablePerformance_Excluded,
  BurstablePerformance_Included,
  BurstablePerformance_Required,
  BurstablePerformance'
  #-}
