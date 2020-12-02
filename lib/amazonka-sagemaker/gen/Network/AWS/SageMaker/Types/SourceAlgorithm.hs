{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithm where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
--
--
--
-- /See:/ 'sourceAlgorithm' smart constructor.
data SourceAlgorithm = SourceAlgorithm'
  { _saModelDataURL ::
      !(Maybe Text),
    _saAlgorithmName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saModelDataURL' - The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- * 'saAlgorithmName' - The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
sourceAlgorithm ::
  -- | 'saAlgorithmName'
  Text ->
  SourceAlgorithm
sourceAlgorithm pAlgorithmName_ =
  SourceAlgorithm'
    { _saModelDataURL = Nothing,
      _saAlgorithmName = pAlgorithmName_
    }

-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
saModelDataURL :: Lens' SourceAlgorithm (Maybe Text)
saModelDataURL = lens _saModelDataURL (\s a -> s {_saModelDataURL = a})

-- | The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
saAlgorithmName :: Lens' SourceAlgorithm Text
saAlgorithmName = lens _saAlgorithmName (\s a -> s {_saAlgorithmName = a})

instance FromJSON SourceAlgorithm where
  parseJSON =
    withObject
      "SourceAlgorithm"
      ( \x ->
          SourceAlgorithm'
            <$> (x .:? "ModelDataUrl") <*> (x .: "AlgorithmName")
      )

instance Hashable SourceAlgorithm

instance NFData SourceAlgorithm

instance ToJSON SourceAlgorithm where
  toJSON SourceAlgorithm' {..} =
    object
      ( catMaybes
          [ ("ModelDataUrl" .=) <$> _saModelDataURL,
            Just ("AlgorithmName" .= _saAlgorithmName)
          ]
      )
