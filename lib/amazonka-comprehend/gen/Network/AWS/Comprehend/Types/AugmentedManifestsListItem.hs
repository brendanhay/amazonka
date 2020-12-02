{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.AugmentedManifestsListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.AugmentedManifestsListItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An augmented manifest file that provides training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
--
--
-- /See:/ 'augmentedManifestsListItem' smart constructor.
data AugmentedManifestsListItem = AugmentedManifestsListItem'
  { _amliS3URI ::
      !Text,
    _amliAttributeNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AugmentedManifestsListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amliS3URI' - The Amazon S3 location of the augmented manifest file.
--
-- * 'amliAttributeNames' - The JSON attribute that contains the annotations for your training documents. The number of attribute names that you specify depends on whether your augmented manifest file is the output of a single labeling job or a chained labeling job. If your file is the output of a single labeling job, specify the LabelAttributeName key that was used when the job was created in Ground Truth. If your file is the output of a chained labeling job, specify the LabelAttributeName key for one or more jobs in the chain. Each LabelAttributeName key provides the annotations from an individual job.
augmentedManifestsListItem ::
  -- | 'amliS3URI'
  Text ->
  AugmentedManifestsListItem
augmentedManifestsListItem pS3URI_ =
  AugmentedManifestsListItem'
    { _amliS3URI = pS3URI_,
      _amliAttributeNames = mempty
    }

-- | The Amazon S3 location of the augmented manifest file.
amliS3URI :: Lens' AugmentedManifestsListItem Text
amliS3URI = lens _amliS3URI (\s a -> s {_amliS3URI = a})

-- | The JSON attribute that contains the annotations for your training documents. The number of attribute names that you specify depends on whether your augmented manifest file is the output of a single labeling job or a chained labeling job. If your file is the output of a single labeling job, specify the LabelAttributeName key that was used when the job was created in Ground Truth. If your file is the output of a chained labeling job, specify the LabelAttributeName key for one or more jobs in the chain. Each LabelAttributeName key provides the annotations from an individual job.
amliAttributeNames :: Lens' AugmentedManifestsListItem [Text]
amliAttributeNames = lens _amliAttributeNames (\s a -> s {_amliAttributeNames = a}) . _Coerce

instance FromJSON AugmentedManifestsListItem where
  parseJSON =
    withObject
      "AugmentedManifestsListItem"
      ( \x ->
          AugmentedManifestsListItem'
            <$> (x .: "S3Uri") <*> (x .:? "AttributeNames" .!= mempty)
      )

instance Hashable AugmentedManifestsListItem

instance NFData AugmentedManifestsListItem

instance ToJSON AugmentedManifestsListItem where
  toJSON AugmentedManifestsListItem' {..} =
    object
      ( catMaybes
          [ Just ("S3Uri" .= _amliS3URI),
            Just ("AttributeNames" .= _amliAttributeNames)
          ]
      )
