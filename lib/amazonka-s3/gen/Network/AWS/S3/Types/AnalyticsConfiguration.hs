{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsFilter
import Network.AWS.S3.Types.StorageClassAnalysis

-- | Specifies the configuration and any analyses for the analytics filter of an Amazon S3 bucket.
--
--
--
-- /See:/ 'analyticsConfiguration' smart constructor.
data AnalyticsConfiguration = AnalyticsConfiguration'
  { _acFilter ::
      !(Maybe AnalyticsFilter),
    _acId :: !Text,
    _acStorageClassAnalysis ::
      !StorageClassAnalysis
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acFilter' - The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- * 'acId' - The ID that identifies the analytics configuration.
--
-- * 'acStorageClassAnalysis' - Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
analyticsConfiguration ::
  -- | 'acId'
  Text ->
  -- | 'acStorageClassAnalysis'
  StorageClassAnalysis ->
  AnalyticsConfiguration
analyticsConfiguration pId_ pStorageClassAnalysis_ =
  AnalyticsConfiguration'
    { _acFilter = Nothing,
      _acId = pId_,
      _acStorageClassAnalysis = pStorageClassAnalysis_
    }

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
acFilter :: Lens' AnalyticsConfiguration (Maybe AnalyticsFilter)
acFilter = lens _acFilter (\s a -> s {_acFilter = a})

-- | The ID that identifies the analytics configuration.
acId :: Lens' AnalyticsConfiguration Text
acId = lens _acId (\s a -> s {_acId = a})

-- | Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
acStorageClassAnalysis :: Lens' AnalyticsConfiguration StorageClassAnalysis
acStorageClassAnalysis = lens _acStorageClassAnalysis (\s a -> s {_acStorageClassAnalysis = a})

instance FromXML AnalyticsConfiguration where
  parseXML x =
    AnalyticsConfiguration'
      <$> (x .@? "Filter") <*> (x .@ "Id") <*> (x .@ "StorageClassAnalysis")

instance Hashable AnalyticsConfiguration

instance NFData AnalyticsConfiguration

instance ToXML AnalyticsConfiguration where
  toXML AnalyticsConfiguration' {..} =
    mconcat
      [ "Filter" @= _acFilter,
        "Id" @= _acId,
        "StorageClassAnalysis" @= _acStorageClassAnalysis
      ]
