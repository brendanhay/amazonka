{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithmSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.SourceAlgorithm

-- | A list of algorithms that were used to create a model package.
--
--
--
-- /See:/ 'sourceAlgorithmSpecification' smart constructor.
newtype SourceAlgorithmSpecification = SourceAlgorithmSpecification'
  { _sasSourceAlgorithms ::
      List1 SourceAlgorithm
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceAlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasSourceAlgorithms' - A list of the algorithms that were used to create a model package.
sourceAlgorithmSpecification ::
  -- | 'sasSourceAlgorithms'
  NonEmpty SourceAlgorithm ->
  SourceAlgorithmSpecification
sourceAlgorithmSpecification pSourceAlgorithms_ =
  SourceAlgorithmSpecification'
    { _sasSourceAlgorithms =
        _List1 # pSourceAlgorithms_
    }

-- | A list of the algorithms that were used to create a model package.
sasSourceAlgorithms :: Lens' SourceAlgorithmSpecification (NonEmpty SourceAlgorithm)
sasSourceAlgorithms = lens _sasSourceAlgorithms (\s a -> s {_sasSourceAlgorithms = a}) . _List1

instance FromJSON SourceAlgorithmSpecification where
  parseJSON =
    withObject
      "SourceAlgorithmSpecification"
      ( \x ->
          SourceAlgorithmSpecification' <$> (x .: "SourceAlgorithms")
      )

instance Hashable SourceAlgorithmSpecification

instance NFData SourceAlgorithmSpecification

instance ToJSON SourceAlgorithmSpecification where
  toJSON SourceAlgorithmSpecification' {..} =
    object
      (catMaybes [Just ("SourceAlgorithms" .= _sasSourceAlgorithms)])
