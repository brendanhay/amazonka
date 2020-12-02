{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CategoricalParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CategoricalParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of categorical hyperparameters to tune.
--
--
--
-- /See:/ 'categoricalParameterRange' smart constructor.
data CategoricalParameterRange = CategoricalParameterRange'
  { _cprName ::
      !Text,
    _cprValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CategoricalParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprName' - The name of the categorical hyperparameter to tune.
--
-- * 'cprValues' - A list of the categories for the hyperparameter.
categoricalParameterRange ::
  -- | 'cprName'
  Text ->
  -- | 'cprValues'
  NonEmpty Text ->
  CategoricalParameterRange
categoricalParameterRange pName_ pValues_ =
  CategoricalParameterRange'
    { _cprName = pName_,
      _cprValues = _List1 # pValues_
    }

-- | The name of the categorical hyperparameter to tune.
cprName :: Lens' CategoricalParameterRange Text
cprName = lens _cprName (\s a -> s {_cprName = a})

-- | A list of the categories for the hyperparameter.
cprValues :: Lens' CategoricalParameterRange (NonEmpty Text)
cprValues = lens _cprValues (\s a -> s {_cprValues = a}) . _List1

instance FromJSON CategoricalParameterRange where
  parseJSON =
    withObject
      "CategoricalParameterRange"
      ( \x ->
          CategoricalParameterRange' <$> (x .: "Name") <*> (x .: "Values")
      )

instance Hashable CategoricalParameterRange

instance NFData CategoricalParameterRange

instance ToJSON CategoricalParameterRange where
  toJSON CategoricalParameterRange' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _cprName), Just ("Values" .= _cprValues)]
      )
