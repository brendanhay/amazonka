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
-- Module      : Network.AWS.ManagedBlockChain.Types.ThresholdComparator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.ThresholdComparator
  ( ThresholdComparator
      ( ..,
        ThresholdComparator_GREATER_THAN,
        ThresholdComparator_GREATER_THAN_OR_EQUAL_TO
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ThresholdComparator = ThresholdComparator'
  { fromThresholdComparator ::
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

pattern ThresholdComparator_GREATER_THAN :: ThresholdComparator
pattern ThresholdComparator_GREATER_THAN = ThresholdComparator' "GREATER_THAN"

pattern ThresholdComparator_GREATER_THAN_OR_EQUAL_TO :: ThresholdComparator
pattern ThresholdComparator_GREATER_THAN_OR_EQUAL_TO = ThresholdComparator' "GREATER_THAN_OR_EQUAL_TO"

{-# COMPLETE
  ThresholdComparator_GREATER_THAN,
  ThresholdComparator_GREATER_THAN_OR_EQUAL_TO,
  ThresholdComparator'
  #-}
