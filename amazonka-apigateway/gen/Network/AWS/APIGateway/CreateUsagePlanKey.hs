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
-- Module      : Network.AWS.APIGateway.CreateUsagePlanKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan key for adding an existing API key to a usage plan.
--
--
module Network.AWS.APIGateway.CreateUsagePlanKey
    (
    -- * Creating a Request
      createUsagePlanKey
    , CreateUsagePlanKey
    -- * Request Lenses
    , cupkUsagePlanId
    , cupkKeyId
    , cupkKeyType

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

-- | The POST request to create a usage plan key for adding an existing API key to a usage plan.
--
--
--
-- /See:/ 'createUsagePlanKey' smart constructor.
data CreateUsagePlanKey = CreateUsagePlanKey'
  { _cupkUsagePlanId :: !Text
  , _cupkKeyId       :: !Text
  , _cupkKeyType     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUsagePlanKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupkUsagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
--
-- * 'cupkKeyId' - [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
--
-- * 'cupkKeyType' - [Required] The type of a 'UsagePlanKey' resource for a plan customer.
createUsagePlanKey
    :: Text -- ^ 'cupkUsagePlanId'
    -> Text -- ^ 'cupkKeyId'
    -> Text -- ^ 'cupkKeyType'
    -> CreateUsagePlanKey
createUsagePlanKey pUsagePlanId_ pKeyId_ pKeyType_ =
  CreateUsagePlanKey'
    { _cupkUsagePlanId = pUsagePlanId_
    , _cupkKeyId = pKeyId_
    , _cupkKeyType = pKeyType_
    }


-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-created 'UsagePlanKey' resource representing a plan customer.
cupkUsagePlanId :: Lens' CreateUsagePlanKey Text
cupkUsagePlanId = lens _cupkUsagePlanId (\ s a -> s{_cupkUsagePlanId = a})

-- | [Required] The identifier of a 'UsagePlanKey' resource for a plan customer.
cupkKeyId :: Lens' CreateUsagePlanKey Text
cupkKeyId = lens _cupkKeyId (\ s a -> s{_cupkKeyId = a})

-- | [Required] The type of a 'UsagePlanKey' resource for a plan customer.
cupkKeyType :: Lens' CreateUsagePlanKey Text
cupkKeyType = lens _cupkKeyType (\ s a -> s{_cupkKeyType = a})

instance AWSRequest CreateUsagePlanKey where
        type Rs CreateUsagePlanKey = UsagePlanKey
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateUsagePlanKey where

instance NFData CreateUsagePlanKey where

instance ToHeaders CreateUsagePlanKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateUsagePlanKey where
        toJSON CreateUsagePlanKey'{..}
          = object
              (catMaybes
                 [Just ("keyId" .= _cupkKeyId),
                  Just ("keyType" .= _cupkKeyType)])

instance ToPath CreateUsagePlanKey where
        toPath CreateUsagePlanKey'{..}
          = mconcat
              ["/usageplans/", toBS _cupkUsagePlanId, "/keys"]

instance ToQuery CreateUsagePlanKey where
        toQuery = const mempty
