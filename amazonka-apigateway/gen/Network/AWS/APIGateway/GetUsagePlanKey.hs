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
-- Module      : Network.AWS.APIGateway.GetUsagePlanKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a usage plan key of a given key identifier.
--
--
module Network.AWS.APIGateway.GetUsagePlanKey
    (
    -- * Creating a Request
      getUsagePlanKey
    , GetUsagePlanKey
    -- * Request Lenses
    , gUsagePlanId
    , gKeyId

    -- * Destructuring the Response
    , usagePlanKey
    , UsagePlanKey
    -- * Response Lenses
    , upkValue
    , upkName
    , upkId
    , upkType
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The GET request to get a usage plan key of a given key identifier.
--
--
--
-- /See:/ 'getUsagePlanKey' smart constructor.
data GetUsagePlanKey = GetUsagePlanKey'
  { _gUsagePlanId :: !Text
  , _gKeyId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUsagePlanKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gUsagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- * 'gKeyId' - [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
getUsagePlanKey
    :: Text -- ^ 'gUsagePlanId'
    -> Text -- ^ 'gKeyId'
    -> GetUsagePlanKey
getUsagePlanKey pUsagePlanId_ pKeyId_ =
  GetUsagePlanKey' {_gUsagePlanId = pUsagePlanId_, _gKeyId = pKeyId_}


-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
gUsagePlanId :: Lens' GetUsagePlanKey Text
gUsagePlanId = lens _gUsagePlanId (\ s a -> s{_gUsagePlanId = a})

-- | [Required] The key Id of the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
gKeyId :: Lens' GetUsagePlanKey Text
gKeyId = lens _gKeyId (\ s a -> s{_gKeyId = a})

instance AWSRequest GetUsagePlanKey where
        type Rs GetUsagePlanKey = UsagePlanKey
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetUsagePlanKey where

instance NFData GetUsagePlanKey where

instance ToHeaders GetUsagePlanKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetUsagePlanKey where
        toPath GetUsagePlanKey'{..}
          = mconcat
              ["/usageplans/", toBS _gUsagePlanId, "/keys/",
               toBS _gKeyId]

instance ToQuery GetUsagePlanKey where
        toQuery = const mempty
