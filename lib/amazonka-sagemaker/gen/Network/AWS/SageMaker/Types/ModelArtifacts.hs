{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelArtifacts where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the location that is configured for storing model artifacts.
--
--
-- Model artifacts are the output that results from training a model, and typically consist of trained parameters, a model defintion that desribes how to compute inferences, and other metadata.
--
--
-- /See:/ 'modelArtifacts' smart constructor.
newtype ModelArtifacts = ModelArtifacts'
  { _maS3ModelArtifacts ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maS3ModelArtifacts' - The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
modelArtifacts ::
  -- | 'maS3ModelArtifacts'
  Text ->
  ModelArtifacts
modelArtifacts pS3ModelArtifacts_ =
  ModelArtifacts' {_maS3ModelArtifacts = pS3ModelArtifacts_}

-- | The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
maS3ModelArtifacts :: Lens' ModelArtifacts Text
maS3ModelArtifacts = lens _maS3ModelArtifacts (\s a -> s {_maS3ModelArtifacts = a})

instance FromJSON ModelArtifacts where
  parseJSON =
    withObject
      "ModelArtifacts"
      (\x -> ModelArtifacts' <$> (x .: "S3ModelArtifacts"))

instance Hashable ModelArtifacts

instance NFData ModelArtifacts
