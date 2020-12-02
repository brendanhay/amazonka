{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TestingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TestingData where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Asset

-- | The dataset used for testing. Optionally, if @AutoCreate@ is set, Amazon Rekognition Custom Labels creates a testing dataset using an 80/20 split of the training dataset.
--
--
--
-- /See:/ 'testingData' smart constructor.
data TestingData = TestingData'
  { _tdAssets :: !(Maybe [Asset]),
    _tdAutoCreate :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestingData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdAssets' - The assets used for testing.
--
-- * 'tdAutoCreate' - If specified, Amazon Rekognition Custom Labels creates a testing dataset with an 80/20 split of the training dataset.
testingData ::
  TestingData
testingData =
  TestingData' {_tdAssets = Nothing, _tdAutoCreate = Nothing}

-- | The assets used for testing.
tdAssets :: Lens' TestingData [Asset]
tdAssets = lens _tdAssets (\s a -> s {_tdAssets = a}) . _Default . _Coerce

-- | If specified, Amazon Rekognition Custom Labels creates a testing dataset with an 80/20 split of the training dataset.
tdAutoCreate :: Lens' TestingData (Maybe Bool)
tdAutoCreate = lens _tdAutoCreate (\s a -> s {_tdAutoCreate = a})

instance FromJSON TestingData where
  parseJSON =
    withObject
      "TestingData"
      ( \x ->
          TestingData'
            <$> (x .:? "Assets" .!= mempty) <*> (x .:? "AutoCreate")
      )

instance Hashable TestingData

instance NFData TestingData

instance ToJSON TestingData where
  toJSON TestingData' {..} =
    object
      ( catMaybes
          [("Assets" .=) <$> _tdAssets, ("AutoCreate" .=) <$> _tdAutoCreate]
      )
