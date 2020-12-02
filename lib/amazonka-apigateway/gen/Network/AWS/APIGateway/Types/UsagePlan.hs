{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.UsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UsagePlan where

import Network.AWS.APIGateway.Types.APIStage
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a usage plan than can specify who can assess associated API stages with specified request limits and quotas.
--
--
-- In a usage plan, you associate an API by specifying the API's Id and a stage name of the specified API. You add plan customers by adding API keys to the plan.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'usagePlan' smart constructor.
data UsagePlan = UsagePlan'
  { _upApiStages :: !(Maybe [APIStage]),
    _upName :: !(Maybe Text),
    _upId :: !(Maybe Text),
    _upThrottle :: !(Maybe ThrottleSettings),
    _upQuota :: !(Maybe QuotaSettings),
    _upDescription :: !(Maybe Text),
    _upProductCode :: !(Maybe Text),
    _upTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsagePlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upApiStages' - The associated API stages of a usage plan.
--
-- * 'upName' - The name of a usage plan.
--
-- * 'upId' - The identifier of a 'UsagePlan' resource.
--
-- * 'upThrottle' - The request throttle limits of a usage plan.
--
-- * 'upQuota' - The maximum number of permitted requests per a given unit time interval.
--
-- * 'upDescription' - The description of a usage plan.
--
-- * 'upProductCode' - The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
--
-- * 'upTags' - The collection of tags. Each tag element is associated with a given resource.
usagePlan ::
  UsagePlan
usagePlan =
  UsagePlan'
    { _upApiStages = Nothing,
      _upName = Nothing,
      _upId = Nothing,
      _upThrottle = Nothing,
      _upQuota = Nothing,
      _upDescription = Nothing,
      _upProductCode = Nothing,
      _upTags = Nothing
    }

-- | The associated API stages of a usage plan.
upApiStages :: Lens' UsagePlan [APIStage]
upApiStages = lens _upApiStages (\s a -> s {_upApiStages = a}) . _Default . _Coerce

-- | The name of a usage plan.
upName :: Lens' UsagePlan (Maybe Text)
upName = lens _upName (\s a -> s {_upName = a})

-- | The identifier of a 'UsagePlan' resource.
upId :: Lens' UsagePlan (Maybe Text)
upId = lens _upId (\s a -> s {_upId = a})

-- | The request throttle limits of a usage plan.
upThrottle :: Lens' UsagePlan (Maybe ThrottleSettings)
upThrottle = lens _upThrottle (\s a -> s {_upThrottle = a})

-- | The maximum number of permitted requests per a given unit time interval.
upQuota :: Lens' UsagePlan (Maybe QuotaSettings)
upQuota = lens _upQuota (\s a -> s {_upQuota = a})

-- | The description of a usage plan.
upDescription :: Lens' UsagePlan (Maybe Text)
upDescription = lens _upDescription (\s a -> s {_upDescription = a})

-- | The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
upProductCode :: Lens' UsagePlan (Maybe Text)
upProductCode = lens _upProductCode (\s a -> s {_upProductCode = a})

-- | The collection of tags. Each tag element is associated with a given resource.
upTags :: Lens' UsagePlan (HashMap Text (Text))
upTags = lens _upTags (\s a -> s {_upTags = a}) . _Default . _Map

instance FromJSON UsagePlan where
  parseJSON =
    withObject
      "UsagePlan"
      ( \x ->
          UsagePlan'
            <$> (x .:? "apiStages" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "throttle")
            <*> (x .:? "quota")
            <*> (x .:? "description")
            <*> (x .:? "productCode")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable UsagePlan

instance NFData UsagePlan
