{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobInputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobInputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.LabelingJobDataAttributes
import Network.AWS.SageMaker.Types.LabelingJobDataSource

-- | Input configuration information for a labeling job.
--
--
--
-- /See:/ 'labelingJobInputConfig' smart constructor.
data LabelingJobInputConfig = LabelingJobInputConfig'
  { _ljicDataAttributes ::
      !(Maybe LabelingJobDataAttributes),
    _ljicDataSource :: !LabelingJobDataSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobInputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljicDataAttributes' - Attributes of the data specified by the customer.
--
-- * 'ljicDataSource' - The location of the input data.
labelingJobInputConfig ::
  -- | 'ljicDataSource'
  LabelingJobDataSource ->
  LabelingJobInputConfig
labelingJobInputConfig pDataSource_ =
  LabelingJobInputConfig'
    { _ljicDataAttributes = Nothing,
      _ljicDataSource = pDataSource_
    }

-- | Attributes of the data specified by the customer.
ljicDataAttributes :: Lens' LabelingJobInputConfig (Maybe LabelingJobDataAttributes)
ljicDataAttributes = lens _ljicDataAttributes (\s a -> s {_ljicDataAttributes = a})

-- | The location of the input data.
ljicDataSource :: Lens' LabelingJobInputConfig LabelingJobDataSource
ljicDataSource = lens _ljicDataSource (\s a -> s {_ljicDataSource = a})

instance FromJSON LabelingJobInputConfig where
  parseJSON =
    withObject
      "LabelingJobInputConfig"
      ( \x ->
          LabelingJobInputConfig'
            <$> (x .:? "DataAttributes") <*> (x .: "DataSource")
      )

instance Hashable LabelingJobInputConfig

instance NFData LabelingJobInputConfig

instance ToJSON LabelingJobInputConfig where
  toJSON LabelingJobInputConfig' {..} =
    object
      ( catMaybes
          [ ("DataAttributes" .=) <$> _ljicDataAttributes,
            Just ("DataSource" .= _ljicDataSource)
          ]
      )
