{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.TriggeringDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.TriggeringDataset where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the dataset whose content generation triggers the new dataset content generation.
--
--
--
-- /See:/ 'triggeringDataset' smart constructor.
newtype TriggeringDataset = TriggeringDataset' {_tdName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TriggeringDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdName' - The name of the dataset whose content generation triggers the new dataset content generation.
triggeringDataset ::
  -- | 'tdName'
  Text ->
  TriggeringDataset
triggeringDataset pName_ = TriggeringDataset' {_tdName = pName_}

-- | The name of the dataset whose content generation triggers the new dataset content generation.
tdName :: Lens' TriggeringDataset Text
tdName = lens _tdName (\s a -> s {_tdName = a})

instance FromJSON TriggeringDataset where
  parseJSON =
    withObject
      "TriggeringDataset"
      (\x -> TriggeringDataset' <$> (x .: "name"))

instance Hashable TriggeringDataset

instance NFData TriggeringDataset

instance ToJSON TriggeringDataset where
  toJSON TriggeringDataset' {..} =
    object (catMaybes [Just ("name" .= _tdName)])
