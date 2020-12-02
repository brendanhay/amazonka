{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ModerationLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ModerationLabel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a single type of unsafe content found in an image or video. Each type of moderated content has a label within a hierarchical taxonomy. For more information, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
--
--
--
-- /See:/ 'moderationLabel' smart constructor.
data ModerationLabel = ModerationLabel'
  { _mlConfidence ::
      !(Maybe Double),
    _mlName :: !(Maybe Text),
    _mlParentName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModerationLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlConfidence' - Specifies the confidence that Amazon Rekognition has that the label has been correctly identified. If you don't specify the @MinConfidence@ parameter in the call to @DetectModerationLabels@ , the operation returns labels with a confidence value greater than or equal to 50 percent.
--
-- * 'mlName' - The label name for the type of unsafe content detected in the image.
--
-- * 'mlParentName' - The name for the parent label. Labels at the top level of the hierarchy have the parent label @""@ .
moderationLabel ::
  ModerationLabel
moderationLabel =
  ModerationLabel'
    { _mlConfidence = Nothing,
      _mlName = Nothing,
      _mlParentName = Nothing
    }

-- | Specifies the confidence that Amazon Rekognition has that the label has been correctly identified. If you don't specify the @MinConfidence@ parameter in the call to @DetectModerationLabels@ , the operation returns labels with a confidence value greater than or equal to 50 percent.
mlConfidence :: Lens' ModerationLabel (Maybe Double)
mlConfidence = lens _mlConfidence (\s a -> s {_mlConfidence = a})

-- | The label name for the type of unsafe content detected in the image.
mlName :: Lens' ModerationLabel (Maybe Text)
mlName = lens _mlName (\s a -> s {_mlName = a})

-- | The name for the parent label. Labels at the top level of the hierarchy have the parent label @""@ .
mlParentName :: Lens' ModerationLabel (Maybe Text)
mlParentName = lens _mlParentName (\s a -> s {_mlParentName = a})

instance FromJSON ModerationLabel where
  parseJSON =
    withObject
      "ModerationLabel"
      ( \x ->
          ModerationLabel'
            <$> (x .:? "Confidence") <*> (x .:? "Name") <*> (x .:? "ParentName")
      )

instance Hashable ModerationLabel

instance NFData ModerationLabel
