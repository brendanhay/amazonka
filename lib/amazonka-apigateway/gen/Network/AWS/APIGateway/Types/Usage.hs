{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Usage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Usage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the usage data of a usage plan.
--
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> , <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage Manage Usage in a Usage Plan>
--
-- /See:/ 'usage' smart constructor.
data Usage = Usage'
  { _uUsagePlanId :: !(Maybe Text),
    _uEndDate :: !(Maybe Text),
    _uItems :: !(Maybe (Map Text ([[Integer]]))),
    _uStartDate :: !(Maybe Text),
    _uPosition :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Usage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uUsagePlanId' - The plan Id associated with this usage data.
--
-- * 'uEndDate' - The ending date of the usage data.
--
-- * 'uItems' - The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
--
-- * 'uStartDate' - The starting date of the usage data.
--
-- * 'uPosition' - Undocumented member.
usage ::
  Usage
usage =
  Usage'
    { _uUsagePlanId = Nothing,
      _uEndDate = Nothing,
      _uItems = Nothing,
      _uStartDate = Nothing,
      _uPosition = Nothing
    }

-- | The plan Id associated with this usage data.
uUsagePlanId :: Lens' Usage (Maybe Text)
uUsagePlanId = lens _uUsagePlanId (\s a -> s {_uUsagePlanId = a})

-- | The ending date of the usage data.
uEndDate :: Lens' Usage (Maybe Text)
uEndDate = lens _uEndDate (\s a -> s {_uEndDate = a})

-- | The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
uItems :: Lens' Usage (HashMap Text ([[Integer]]))
uItems = lens _uItems (\s a -> s {_uItems = a}) . _Default . _Map

-- | The starting date of the usage data.
uStartDate :: Lens' Usage (Maybe Text)
uStartDate = lens _uStartDate (\s a -> s {_uStartDate = a})

-- | Undocumented member.
uPosition :: Lens' Usage (Maybe Text)
uPosition = lens _uPosition (\s a -> s {_uPosition = a})

instance FromJSON Usage where
  parseJSON =
    withObject
      "Usage"
      ( \x ->
          Usage'
            <$> (x .:? "usagePlanId")
            <*> (x .:? "endDate")
            <*> (x .:? "values" .!= mempty)
            <*> (x .:? "startDate")
            <*> (x .:? "position")
      )

instance Hashable Usage

instance NFData Usage
