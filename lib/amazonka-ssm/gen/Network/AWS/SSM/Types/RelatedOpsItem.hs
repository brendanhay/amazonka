{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.RelatedOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.RelatedOpsItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An OpsItems that shares something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
--
--
-- /See:/ 'relatedOpsItem' smart constructor.
newtype RelatedOpsItem = RelatedOpsItem' {_roiOpsItemId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelatedOpsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roiOpsItemId' - The ID of an OpsItem related to the current OpsItem.
relatedOpsItem ::
  -- | 'roiOpsItemId'
  Text ->
  RelatedOpsItem
relatedOpsItem pOpsItemId_ =
  RelatedOpsItem' {_roiOpsItemId = pOpsItemId_}

-- | The ID of an OpsItem related to the current OpsItem.
roiOpsItemId :: Lens' RelatedOpsItem Text
roiOpsItemId = lens _roiOpsItemId (\s a -> s {_roiOpsItemId = a})

instance FromJSON RelatedOpsItem where
  parseJSON =
    withObject
      "RelatedOpsItem"
      (\x -> RelatedOpsItem' <$> (x .: "OpsItemId"))

instance Hashable RelatedOpsItem

instance NFData RelatedOpsItem

instance ToJSON RelatedOpsItem where
  toJSON RelatedOpsItem' {..} =
    object (catMaybes [Just ("OpsItemId" .= _roiOpsItemId)])
