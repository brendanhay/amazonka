-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Algorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Algorithm
  ( Algorithm
      ( Algorithm',
        SGD
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The function used to train an @MLModel@ . Training choices supported by Amazon ML include the following:
--
--
--     * @SGD@ - Stochastic Gradient Descent.
--
--     * @RandomForest@ - Random forest of decision trees.
newtype Algorithm = Algorithm' Lude.Text
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

pattern SGD :: Algorithm
pattern SGD = Algorithm' "sgd"

{-# COMPLETE
  SGD,
  Algorithm'
  #-}
