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
-- Module      : Amazonka.Rekognition.Types.QualityFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.QualityFilter
  ( QualityFilter
      ( ..,
        QualityFilter_AUTO,
        QualityFilter_HIGH,
        QualityFilter_LOW,
        QualityFilter_MEDIUM,
        QualityFilter_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QualityFilter = QualityFilter'
  { fromQualityFilter ::
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

pattern QualityFilter_AUTO :: QualityFilter
pattern QualityFilter_AUTO = QualityFilter' "AUTO"

pattern QualityFilter_HIGH :: QualityFilter
pattern QualityFilter_HIGH = QualityFilter' "HIGH"

pattern QualityFilter_LOW :: QualityFilter
pattern QualityFilter_LOW = QualityFilter' "LOW"

pattern QualityFilter_MEDIUM :: QualityFilter
pattern QualityFilter_MEDIUM = QualityFilter' "MEDIUM"

pattern QualityFilter_NONE :: QualityFilter
pattern QualityFilter_NONE = QualityFilter' "NONE"

{-# COMPLETE
  QualityFilter_AUTO,
  QualityFilter_HIGH,
  QualityFilter_LOW,
  QualityFilter_MEDIUM,
  QualityFilter_NONE,
  QualityFilter'
  #-}
