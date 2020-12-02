{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The source of the experiment.
--
--
--
-- /See:/ 'experimentSource' smart constructor.
data ExperimentSource = ExperimentSource'
  { _esSourceType ::
      !(Maybe Text),
    _esSourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExperimentSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esSourceType' - The source type.
--
-- * 'esSourceARN' - The Amazon Resource Name (ARN) of the source.
experimentSource ::
  -- | 'esSourceARN'
  Text ->
  ExperimentSource
experimentSource pSourceARN_ =
  ExperimentSource'
    { _esSourceType = Nothing,
      _esSourceARN = pSourceARN_
    }

-- | The source type.
esSourceType :: Lens' ExperimentSource (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s {_esSourceType = a})

-- | The Amazon Resource Name (ARN) of the source.
esSourceARN :: Lens' ExperimentSource Text
esSourceARN = lens _esSourceARN (\s a -> s {_esSourceARN = a})

instance FromJSON ExperimentSource where
  parseJSON =
    withObject
      "ExperimentSource"
      ( \x ->
          ExperimentSource' <$> (x .:? "SourceType") <*> (x .: "SourceArn")
      )

instance Hashable ExperimentSource

instance NFData ExperimentSource
