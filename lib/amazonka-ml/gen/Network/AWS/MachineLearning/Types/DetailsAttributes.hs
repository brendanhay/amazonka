{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DetailsAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DetailsAttributes
  ( DetailsAttributes
      ( DetailsAttributes',
        PredictiveModelType,
        Algorithm
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Contains the key values of @DetailsMap@ : @PredictiveModelType@ - Indicates the type of the @MLModel@ . @Algorithm@ - Indicates the algorithm that was used for the @MLModel@ .
newtype DetailsAttributes = DetailsAttributes' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PredictiveModelType :: DetailsAttributes
pattern PredictiveModelType = DetailsAttributes' "PredictiveModelType"

pattern Algorithm :: DetailsAttributes
pattern Algorithm = DetailsAttributes' "Algorithm"

{-# COMPLETE
  PredictiveModelType,
  Algorithm,
  DetailsAttributes'
  #-}
