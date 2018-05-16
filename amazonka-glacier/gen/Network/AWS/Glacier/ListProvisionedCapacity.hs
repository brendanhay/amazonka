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
-- Module      : Network.AWS.Glacier.ListProvisionedCapacity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the provisioned capacity units for the specified AWS account.
--
--
module Network.AWS.Glacier.ListProvisionedCapacity
    (
    -- * Creating a Request
      listProvisionedCapacity
    , ListProvisionedCapacity
    -- * Request Lenses
    , lpcAccountId

    -- * Destructuring the Response
    , listProvisionedCapacityResponse
    , ListProvisionedCapacityResponse
    -- * Response Lenses
    , lpcrsProvisionedCapacityList
    , lpcrsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProvisionedCapacity' smart constructor.
newtype ListProvisionedCapacity = ListProvisionedCapacity'
  { _lpcAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProvisionedCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpcAccountId' - The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
listProvisionedCapacity
    :: Text -- ^ 'lpcAccountId'
    -> ListProvisionedCapacity
listProvisionedCapacity pAccountId_ =
  ListProvisionedCapacity' {_lpcAccountId = pAccountId_}


-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
lpcAccountId :: Lens' ListProvisionedCapacity Text
lpcAccountId = lens _lpcAccountId (\ s a -> s{_lpcAccountId = a})

instance AWSRequest ListProvisionedCapacity where
        type Rs ListProvisionedCapacity =
             ListProvisionedCapacityResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 ListProvisionedCapacityResponse' <$>
                   (x .?> "ProvisionedCapacityList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListProvisionedCapacity where

instance NFData ListProvisionedCapacity where

instance ToHeaders ListProvisionedCapacity where
        toHeaders = const mempty

instance ToPath ListProvisionedCapacity where
        toPath ListProvisionedCapacity'{..}
          = mconcat
              ["/", toBS _lpcAccountId, "/provisioned-capacity"]

instance ToQuery ListProvisionedCapacity where
        toQuery = const mempty

-- | /See:/ 'listProvisionedCapacityResponse' smart constructor.
data ListProvisionedCapacityResponse = ListProvisionedCapacityResponse'
  { _lpcrsProvisionedCapacityList :: !(Maybe [ProvisionedCapacityDescription])
  , _lpcrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProvisionedCapacityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpcrsProvisionedCapacityList' - The response body contains the following JSON fields.
--
-- * 'lpcrsResponseStatus' - -- | The response status code.
listProvisionedCapacityResponse
    :: Int -- ^ 'lpcrsResponseStatus'
    -> ListProvisionedCapacityResponse
listProvisionedCapacityResponse pResponseStatus_ =
  ListProvisionedCapacityResponse'
    { _lpcrsProvisionedCapacityList = Nothing
    , _lpcrsResponseStatus = pResponseStatus_
    }


-- | The response body contains the following JSON fields.
lpcrsProvisionedCapacityList :: Lens' ListProvisionedCapacityResponse [ProvisionedCapacityDescription]
lpcrsProvisionedCapacityList = lens _lpcrsProvisionedCapacityList (\ s a -> s{_lpcrsProvisionedCapacityList = a}) . _Default . _Coerce

-- | -- | The response status code.
lpcrsResponseStatus :: Lens' ListProvisionedCapacityResponse Int
lpcrsResponseStatus = lens _lpcrsResponseStatus (\ s a -> s{_lpcrsResponseStatus = a})

instance NFData ListProvisionedCapacityResponse where
