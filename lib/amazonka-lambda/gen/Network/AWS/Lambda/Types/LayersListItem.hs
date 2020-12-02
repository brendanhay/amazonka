{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayersListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayersListItem where

import Network.AWS.Lambda.Types.LayerVersionsListItem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layersListItem' smart constructor.
data LayersListItem = LayersListItem'
  { _lliLayerName ::
      !(Maybe Text),
    _lliLatestMatchingVersion :: !(Maybe LayerVersionsListItem),
    _lliLayerARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LayersListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lliLayerName' - The name of the layer.
--
-- * 'lliLatestMatchingVersion' - The newest version of the layer.
--
-- * 'lliLayerARN' - The Amazon Resource Name (ARN) of the function layer.
layersListItem ::
  LayersListItem
layersListItem =
  LayersListItem'
    { _lliLayerName = Nothing,
      _lliLatestMatchingVersion = Nothing,
      _lliLayerARN = Nothing
    }

-- | The name of the layer.
lliLayerName :: Lens' LayersListItem (Maybe Text)
lliLayerName = lens _lliLayerName (\s a -> s {_lliLayerName = a})

-- | The newest version of the layer.
lliLatestMatchingVersion :: Lens' LayersListItem (Maybe LayerVersionsListItem)
lliLatestMatchingVersion = lens _lliLatestMatchingVersion (\s a -> s {_lliLatestMatchingVersion = a})

-- | The Amazon Resource Name (ARN) of the function layer.
lliLayerARN :: Lens' LayersListItem (Maybe Text)
lliLayerARN = lens _lliLayerARN (\s a -> s {_lliLayerARN = a})

instance FromJSON LayersListItem where
  parseJSON =
    withObject
      "LayersListItem"
      ( \x ->
          LayersListItem'
            <$> (x .:? "LayerName")
            <*> (x .:? "LatestMatchingVersion")
            <*> (x .:? "LayerArn")
      )

instance Hashable LayersListItem

instance NFData LayersListItem
