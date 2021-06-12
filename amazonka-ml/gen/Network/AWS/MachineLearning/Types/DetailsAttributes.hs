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
-- Module      : Network.AWS.MachineLearning.Types.DetailsAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DetailsAttributes
  ( DetailsAttributes
      ( ..,
        DetailsAttributes_Algorithm,
        DetailsAttributes_PredictiveModelType
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Contains the key values of @DetailsMap@: @PredictiveModelType@ -
-- Indicates the type of the @MLModel@. @Algorithm@ - Indicates the
-- algorithm that was used for the @MLModel@.
newtype DetailsAttributes = DetailsAttributes'
  { fromDetailsAttributes ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DetailsAttributes_Algorithm :: DetailsAttributes
pattern DetailsAttributes_Algorithm = DetailsAttributes' "Algorithm"

pattern DetailsAttributes_PredictiveModelType :: DetailsAttributes
pattern DetailsAttributes_PredictiveModelType = DetailsAttributes' "PredictiveModelType"

{-# COMPLETE
  DetailsAttributes_Algorithm,
  DetailsAttributes_PredictiveModelType,
  DetailsAttributes'
  #-}
