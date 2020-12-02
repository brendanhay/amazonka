{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus where

import Network.AWS.CostExplorer.Types.CostCategoryStatus
import Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The list of processing statuses for Cost Management products for a specific cost category.
--
--
--
-- /See:/ 'costCategoryProcessingStatus' smart constructor.
data CostCategoryProcessingStatus = CostCategoryProcessingStatus'
  { _ccpsStatus ::
      !(Maybe CostCategoryStatus),
    _ccpsComponent ::
      !( Maybe
           CostCategoryStatusComponent
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CostCategoryProcessingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpsStatus' - The process status for a specific cost category.
--
-- * 'ccpsComponent' - The Cost Management product name of the applied status.
costCategoryProcessingStatus ::
  CostCategoryProcessingStatus
costCategoryProcessingStatus =
  CostCategoryProcessingStatus'
    { _ccpsStatus = Nothing,
      _ccpsComponent = Nothing
    }

-- | The process status for a specific cost category.
ccpsStatus :: Lens' CostCategoryProcessingStatus (Maybe CostCategoryStatus)
ccpsStatus = lens _ccpsStatus (\s a -> s {_ccpsStatus = a})

-- | The Cost Management product name of the applied status.
ccpsComponent :: Lens' CostCategoryProcessingStatus (Maybe CostCategoryStatusComponent)
ccpsComponent = lens _ccpsComponent (\s a -> s {_ccpsComponent = a})

instance FromJSON CostCategoryProcessingStatus where
  parseJSON =
    withObject
      "CostCategoryProcessingStatus"
      ( \x ->
          CostCategoryProcessingStatus'
            <$> (x .:? "Status") <*> (x .:? "Component")
      )

instance Hashable CostCategoryProcessingStatus

instance NFData CostCategoryProcessingStatus
