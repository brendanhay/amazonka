{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateUsagePlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan with the throttle and quota limits, as well as the associated API stages, specified in the payload.
--
--
module Network.AWS.APIGateway.CreateUsagePlan
    (
    -- * Creating a Request
      createUsagePlan
    , CreateUsagePlan
    -- * Request Lenses
    , cupApiStages
    , cupThrottle
    , cupQuota
    , cupDescription
    , cupName

    -- * Destructuring the Response
    , usagePlan
    , UsagePlan
    -- * Response Lenses
    , upApiStages
    , upName
    , upId
    , upThrottle
    , upQuota
    , upDescription
    , upProductCode
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The POST request to create a usage plan with the name, description, throttle limits and quota limits, as well as the associated API stages, specified in the payload.
--
--
--
-- /See:/ 'createUsagePlan' smart constructor.
data CreateUsagePlan = CreateUsagePlan'
  { _cupApiStages   :: !(Maybe [APIStage])
  , _cupThrottle    :: !(Maybe ThrottleSettings)
  , _cupQuota       :: !(Maybe QuotaSettings)
  , _cupDescription :: !(Maybe Text)
  , _cupName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUsagePlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupApiStages' - The associated API stages of the usage plan.
--
-- * 'cupThrottle' - The throttling limits of the usage plan.
--
-- * 'cupQuota' - The quota of the usage plan.
--
-- * 'cupDescription' - The description of the usage plan.
--
-- * 'cupName' - [Required] The name of the usage plan.
createUsagePlan
    :: Text -- ^ 'cupName'
    -> CreateUsagePlan
createUsagePlan pName_ =
  CreateUsagePlan'
    { _cupApiStages = Nothing
    , _cupThrottle = Nothing
    , _cupQuota = Nothing
    , _cupDescription = Nothing
    , _cupName = pName_
    }


-- | The associated API stages of the usage plan.
cupApiStages :: Lens' CreateUsagePlan [APIStage]
cupApiStages = lens _cupApiStages (\ s a -> s{_cupApiStages = a}) . _Default . _Coerce

-- | The throttling limits of the usage plan.
cupThrottle :: Lens' CreateUsagePlan (Maybe ThrottleSettings)
cupThrottle = lens _cupThrottle (\ s a -> s{_cupThrottle = a})

-- | The quota of the usage plan.
cupQuota :: Lens' CreateUsagePlan (Maybe QuotaSettings)
cupQuota = lens _cupQuota (\ s a -> s{_cupQuota = a})

-- | The description of the usage plan.
cupDescription :: Lens' CreateUsagePlan (Maybe Text)
cupDescription = lens _cupDescription (\ s a -> s{_cupDescription = a})

-- | [Required] The name of the usage plan.
cupName :: Lens' CreateUsagePlan Text
cupName = lens _cupName (\ s a -> s{_cupName = a})

instance AWSRequest CreateUsagePlan where
        type Rs CreateUsagePlan = UsagePlan
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateUsagePlan where

instance NFData CreateUsagePlan where

instance ToHeaders CreateUsagePlan where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateUsagePlan where
        toJSON CreateUsagePlan'{..}
          = object
              (catMaybes
                 [("apiStages" .=) <$> _cupApiStages,
                  ("throttle" .=) <$> _cupThrottle,
                  ("quota" .=) <$> _cupQuota,
                  ("description" .=) <$> _cupDescription,
                  Just ("name" .= _cupName)])

instance ToPath CreateUsagePlan where
        toPath = const "/usageplans"

instance ToQuery CreateUsagePlan where
        toQuery = const mempty
