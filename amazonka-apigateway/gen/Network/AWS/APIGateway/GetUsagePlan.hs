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
-- Module      : Network.AWS.APIGateway.GetUsagePlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan of a given plan identifier.
--
--
module Network.AWS.APIGateway.GetUsagePlan
    (
    -- * Creating a Request
      getUsagePlan
    , GetUsagePlan
    -- * Request Lenses
    , gupUsagePlanId

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

-- | The GET request to get a usage plan of a given plan identifier.
--
--
--
-- /See:/ 'getUsagePlan' smart constructor.
newtype GetUsagePlan = GetUsagePlan'
  { _gupUsagePlanId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUsagePlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gupUsagePlanId' - [Required] The identifier of the 'UsagePlan' resource to be retrieved.
getUsagePlan
    :: Text -- ^ 'gupUsagePlanId'
    -> GetUsagePlan
getUsagePlan pUsagePlanId_ = GetUsagePlan' {_gupUsagePlanId = pUsagePlanId_}


-- | [Required] The identifier of the 'UsagePlan' resource to be retrieved.
gupUsagePlanId :: Lens' GetUsagePlan Text
gupUsagePlanId = lens _gupUsagePlanId (\ s a -> s{_gupUsagePlanId = a})

instance AWSRequest GetUsagePlan where
        type Rs GetUsagePlan = UsagePlan
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetUsagePlan where

instance NFData GetUsagePlan where

instance ToHeaders GetUsagePlan where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetUsagePlan where
        toPath GetUsagePlan'{..}
          = mconcat ["/usageplans/", toBS _gupUsagePlanId]

instance ToQuery GetUsagePlan where
        toQuery = const mempty
