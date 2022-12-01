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
-- Module      : Amazonka.SESV2.Types.MetricDimensionName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.MetricDimensionName
  ( MetricDimensionName
      ( ..,
        MetricDimensionName_CONFIGURATION_SET,
        MetricDimensionName_EMAIL_IDENTITY,
        MetricDimensionName_ISP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The @BatchGetMetricDataQuery@ dimension name. This can be one of the
-- following:
--
-- -   @EMAIL_IDENTITY@ – The email identity used when sending messages.
--
-- -   @CONFIGURATION_SET@ – The configuration set used when sending
--     messages (if one was used).
--
-- -   @ISP@ – The recipient ISP (e.g. @Gmail@, @Yahoo@, etc.).
newtype MetricDimensionName = MetricDimensionName'
  { fromMetricDimensionName ::
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

pattern MetricDimensionName_CONFIGURATION_SET :: MetricDimensionName
pattern MetricDimensionName_CONFIGURATION_SET = MetricDimensionName' "CONFIGURATION_SET"

pattern MetricDimensionName_EMAIL_IDENTITY :: MetricDimensionName
pattern MetricDimensionName_EMAIL_IDENTITY = MetricDimensionName' "EMAIL_IDENTITY"

pattern MetricDimensionName_ISP :: MetricDimensionName
pattern MetricDimensionName_ISP = MetricDimensionName' "ISP"

{-# COMPLETE
  MetricDimensionName_CONFIGURATION_SET,
  MetricDimensionName_EMAIL_IDENTITY,
  MetricDimensionName_ISP,
  MetricDimensionName'
  #-}
