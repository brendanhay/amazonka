{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.QualityFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.QualityFilter
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

import qualified Network.AWS.Prelude as Prelude

newtype QualityFilter = QualityFilter'
  { fromQualityFilter ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
