{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CollectionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CollectionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for tensor collections.
--
--
--
-- /See:/ 'collectionConfiguration' smart constructor.
data CollectionConfiguration = CollectionConfiguration'
  { _ccCollectionParameters ::
      !(Maybe (Map Text (Text))),
    _ccCollectionName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CollectionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCollectionParameters' - Parameter values for the tensor collection. The allowed parameters are @"name"@ , @"include_regex"@ , @"reduction_config"@ , @"save_config"@ , @"tensor_names"@ , and @"save_histogram"@ .
--
-- * 'ccCollectionName' - The name of the tensor collection. The name must be unique relative to other rule configuration names.
collectionConfiguration ::
  CollectionConfiguration
collectionConfiguration =
  CollectionConfiguration'
    { _ccCollectionParameters = Nothing,
      _ccCollectionName = Nothing
    }

-- | Parameter values for the tensor collection. The allowed parameters are @"name"@ , @"include_regex"@ , @"reduction_config"@ , @"save_config"@ , @"tensor_names"@ , and @"save_histogram"@ .
ccCollectionParameters :: Lens' CollectionConfiguration (HashMap Text (Text))
ccCollectionParameters = lens _ccCollectionParameters (\s a -> s {_ccCollectionParameters = a}) . _Default . _Map

-- | The name of the tensor collection. The name must be unique relative to other rule configuration names.
ccCollectionName :: Lens' CollectionConfiguration (Maybe Text)
ccCollectionName = lens _ccCollectionName (\s a -> s {_ccCollectionName = a})

instance FromJSON CollectionConfiguration where
  parseJSON =
    withObject
      "CollectionConfiguration"
      ( \x ->
          CollectionConfiguration'
            <$> (x .:? "CollectionParameters" .!= mempty)
            <*> (x .:? "CollectionName")
      )

instance Hashable CollectionConfiguration

instance NFData CollectionConfiguration

instance ToJSON CollectionConfiguration where
  toJSON CollectionConfiguration' {..} =
    object
      ( catMaybes
          [ ("CollectionParameters" .=) <$> _ccCollectionParameters,
            ("CollectionName" .=) <$> _ccCollectionName
          ]
      )
