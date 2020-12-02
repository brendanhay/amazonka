{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformParameters where

import Network.AWS.Glue.Types.FindMatchesParameters
import Network.AWS.Glue.Types.TransformType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The algorithm-specific parameters that are associated with the machine learning transform.
--
--
--
-- /See:/ 'transformParameters' smart constructor.
data TransformParameters = TransformParameters'
  { _tpFindMatchesParameters ::
      !(Maybe FindMatchesParameters),
    _tpTransformType :: !TransformType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpFindMatchesParameters' - The parameters for the find matches algorithm.
--
-- * 'tpTransformType' - The type of machine learning transform. For information about the types of machine learning transforms, see <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms> .
transformParameters ::
  -- | 'tpTransformType'
  TransformType ->
  TransformParameters
transformParameters pTransformType_ =
  TransformParameters'
    { _tpFindMatchesParameters = Nothing,
      _tpTransformType = pTransformType_
    }

-- | The parameters for the find matches algorithm.
tpFindMatchesParameters :: Lens' TransformParameters (Maybe FindMatchesParameters)
tpFindMatchesParameters = lens _tpFindMatchesParameters (\s a -> s {_tpFindMatchesParameters = a})

-- | The type of machine learning transform. For information about the types of machine learning transforms, see <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms> .
tpTransformType :: Lens' TransformParameters TransformType
tpTransformType = lens _tpTransformType (\s a -> s {_tpTransformType = a})

instance FromJSON TransformParameters where
  parseJSON =
    withObject
      "TransformParameters"
      ( \x ->
          TransformParameters'
            <$> (x .:? "FindMatchesParameters") <*> (x .: "TransformType")
      )

instance Hashable TransformParameters

instance NFData TransformParameters

instance ToJSON TransformParameters where
  toJSON TransformParameters' {..} =
    object
      ( catMaybes
          [ ("FindMatchesParameters" .=) <$> _tpFindMatchesParameters,
            Just ("TransformType" .= _tpTransformType)
          ]
      )
