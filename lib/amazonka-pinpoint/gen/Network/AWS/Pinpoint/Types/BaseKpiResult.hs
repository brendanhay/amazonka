{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaseKpiResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaseKpiResult where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ResultRow
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
--
--
-- /See:/ 'baseKpiResult' smart constructor.
newtype BaseKpiResult = BaseKpiResult' {_bkrRows :: [ResultRow]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BaseKpiResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bkrRows' - An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
baseKpiResult ::
  BaseKpiResult
baseKpiResult = BaseKpiResult' {_bkrRows = mempty}

-- | An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
bkrRows :: Lens' BaseKpiResult [ResultRow]
bkrRows = lens _bkrRows (\s a -> s {_bkrRows = a}) . _Coerce

instance FromJSON BaseKpiResult where
  parseJSON =
    withObject
      "BaseKpiResult"
      (\x -> BaseKpiResult' <$> (x .:? "Rows" .!= mempty))

instance Hashable BaseKpiResult

instance NFData BaseKpiResult
