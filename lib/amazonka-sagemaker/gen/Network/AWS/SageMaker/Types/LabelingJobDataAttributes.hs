{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobDataAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ContentClassifier

-- | Attributes of the data specified by the customer. Use these to describe the data to be labeled.
--
--
--
-- /See:/ 'labelingJobDataAttributes' smart constructor.
newtype LabelingJobDataAttributes = LabelingJobDataAttributes'
  { _ljdaContentClassifiers ::
      Maybe [ContentClassifier]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobDataAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljdaContentClassifiers' - Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
labelingJobDataAttributes ::
  LabelingJobDataAttributes
labelingJobDataAttributes =
  LabelingJobDataAttributes' {_ljdaContentClassifiers = Nothing}

-- | Declares that your content is free of personally identifiable information or adult content. Amazon SageMaker may restrict the Amazon Mechanical Turk workers that can view your task based on this information.
ljdaContentClassifiers :: Lens' LabelingJobDataAttributes [ContentClassifier]
ljdaContentClassifiers = lens _ljdaContentClassifiers (\s a -> s {_ljdaContentClassifiers = a}) . _Default . _Coerce

instance FromJSON LabelingJobDataAttributes where
  parseJSON =
    withObject
      "LabelingJobDataAttributes"
      ( \x ->
          LabelingJobDataAttributes'
            <$> (x .:? "ContentClassifiers" .!= mempty)
      )

instance Hashable LabelingJobDataAttributes

instance NFData LabelingJobDataAttributes

instance ToJSON LabelingJobDataAttributes where
  toJSON LabelingJobDataAttributes' {..} =
    object
      (catMaybes [("ContentClassifiers" .=) <$> _ljdaContentClassifiers])
