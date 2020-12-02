{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the possible values for a categorical hyperparameter.
--
--
--
-- /See:/ 'categoricalParameterRangeSpecification' smart constructor.
newtype CategoricalParameterRangeSpecification = CategoricalParameterRangeSpecification'
  { _cprsValues ::
      List1 Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CategoricalParameterRangeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsValues' - The allowed categories for the hyperparameter.
categoricalParameterRangeSpecification ::
  -- | 'cprsValues'
  NonEmpty Text ->
  CategoricalParameterRangeSpecification
categoricalParameterRangeSpecification pValues_ =
  CategoricalParameterRangeSpecification'
    { _cprsValues =
        _List1 # pValues_
    }

-- | The allowed categories for the hyperparameter.
cprsValues :: Lens' CategoricalParameterRangeSpecification (NonEmpty Text)
cprsValues = lens _cprsValues (\s a -> s {_cprsValues = a}) . _List1

instance FromJSON CategoricalParameterRangeSpecification where
  parseJSON =
    withObject
      "CategoricalParameterRangeSpecification"
      ( \x ->
          CategoricalParameterRangeSpecification' <$> (x .: "Values")
      )

instance Hashable CategoricalParameterRangeSpecification

instance NFData CategoricalParameterRangeSpecification

instance ToJSON CategoricalParameterRangeSpecification where
  toJSON CategoricalParameterRangeSpecification' {..} =
    object (catMaybes [Just ("Values" .= _cprsValues)])
