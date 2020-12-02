{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentArtifact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an input or output artifact of a trial component. You specify @TrialComponentArtifact@ as part of the @InputArtifacts@ and @OutputArtifacts@ parameters in the 'CreateTrialComponent' request.
--
--
-- Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types. Examples of output artifacts are metrics, snapshots, logs, and images.
--
--
-- /See:/ 'trialComponentArtifact' smart constructor.
data TrialComponentArtifact = TrialComponentArtifact'
  { _tcaMediaType ::
      !(Maybe Text),
    _tcaValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialComponentArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcaMediaType' - The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
--
-- * 'tcaValue' - The location of the artifact.
trialComponentArtifact ::
  -- | 'tcaValue'
  Text ->
  TrialComponentArtifact
trialComponentArtifact pValue_ =
  TrialComponentArtifact'
    { _tcaMediaType = Nothing,
      _tcaValue = pValue_
    }

-- | The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
tcaMediaType :: Lens' TrialComponentArtifact (Maybe Text)
tcaMediaType = lens _tcaMediaType (\s a -> s {_tcaMediaType = a})

-- | The location of the artifact.
tcaValue :: Lens' TrialComponentArtifact Text
tcaValue = lens _tcaValue (\s a -> s {_tcaValue = a})

instance FromJSON TrialComponentArtifact where
  parseJSON =
    withObject
      "TrialComponentArtifact"
      ( \x ->
          TrialComponentArtifact' <$> (x .:? "MediaType") <*> (x .: "Value")
      )

instance Hashable TrialComponentArtifact

instance NFData TrialComponentArtifact

instance ToJSON TrialComponentArtifact where
  toJSON TrialComponentArtifact' {..} =
    object
      ( catMaybes
          [("MediaType" .=) <$> _tcaMediaType, Just ("Value" .= _tcaValue)]
      )
