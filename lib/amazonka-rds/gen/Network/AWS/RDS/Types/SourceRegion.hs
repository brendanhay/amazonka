{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.SourceRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.SourceRegion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains an AWS Region name as the result of a successful call to the @DescribeSourceRegions@ action.
--
--
--
-- /See:/ 'sourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { _srStatus :: !(Maybe Text),
    _srRegionName :: !(Maybe Text),
    _srEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceRegion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus' - The status of the source AWS Region.
--
-- * 'srRegionName' - The name of the source AWS Region.
--
-- * 'srEndpoint' - The endpoint for the source AWS Region endpoint.
sourceRegion ::
  SourceRegion
sourceRegion =
  SourceRegion'
    { _srStatus = Nothing,
      _srRegionName = Nothing,
      _srEndpoint = Nothing
    }

-- | The status of the source AWS Region.
srStatus :: Lens' SourceRegion (Maybe Text)
srStatus = lens _srStatus (\s a -> s {_srStatus = a})

-- | The name of the source AWS Region.
srRegionName :: Lens' SourceRegion (Maybe Text)
srRegionName = lens _srRegionName (\s a -> s {_srRegionName = a})

-- | The endpoint for the source AWS Region endpoint.
srEndpoint :: Lens' SourceRegion (Maybe Text)
srEndpoint = lens _srEndpoint (\s a -> s {_srEndpoint = a})

instance FromXML SourceRegion where
  parseXML x =
    SourceRegion'
      <$> (x .@? "Status") <*> (x .@? "RegionName") <*> (x .@? "Endpoint")

instance Hashable SourceRegion

instance NFData SourceRegion
