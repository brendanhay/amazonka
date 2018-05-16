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
-- Module      : Network.AWS.CodeDeploy.ListOnPremisesInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of names for one or more on-premises instances.
--
--
-- Unless otherwise specified, both registered and deregistered on-premises instance names will be listed. To list only registered or deregistered on-premises instance names, use the registration status parameter.
--
module Network.AWS.CodeDeploy.ListOnPremisesInstances
    (
    -- * Creating a Request
      listOnPremisesInstances
    , ListOnPremisesInstances
    -- * Request Lenses
    , lopiTagFilters
    , lopiNextToken
    , lopiRegistrationStatus

    -- * Destructuring the Response
    , listOnPremisesInstancesResponse
    , ListOnPremisesInstancesResponse
    -- * Response Lenses
    , lopirsNextToken
    , lopirsInstanceNames
    , lopirsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListOnPremisesInstances operation.
--
--
--
-- /See:/ 'listOnPremisesInstances' smart constructor.
data ListOnPremisesInstances = ListOnPremisesInstances'
  { _lopiTagFilters         :: !(Maybe [TagFilter])
  , _lopiNextToken          :: !(Maybe Text)
  , _lopiRegistrationStatus :: !(Maybe RegistrationStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOnPremisesInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopiTagFilters' - The on-premises instance tags that will be used to restrict the corresponding on-premises instance names returned.
--
-- * 'lopiNextToken' - An identifier returned from the previous list on-premises instances call. It can be used to return the next set of on-premises instances in the list.
--
-- * 'lopiRegistrationStatus' - The registration status of the on-premises instances:     * Deregistered: Include deregistered on-premises instances in the resulting list.     * Registered: Include registered on-premises instances in the resulting list.
listOnPremisesInstances
    :: ListOnPremisesInstances
listOnPremisesInstances =
  ListOnPremisesInstances'
    { _lopiTagFilters = Nothing
    , _lopiNextToken = Nothing
    , _lopiRegistrationStatus = Nothing
    }


-- | The on-premises instance tags that will be used to restrict the corresponding on-premises instance names returned.
lopiTagFilters :: Lens' ListOnPremisesInstances [TagFilter]
lopiTagFilters = lens _lopiTagFilters (\ s a -> s{_lopiTagFilters = a}) . _Default . _Coerce

-- | An identifier returned from the previous list on-premises instances call. It can be used to return the next set of on-premises instances in the list.
lopiNextToken :: Lens' ListOnPremisesInstances (Maybe Text)
lopiNextToken = lens _lopiNextToken (\ s a -> s{_lopiNextToken = a})

-- | The registration status of the on-premises instances:     * Deregistered: Include deregistered on-premises instances in the resulting list.     * Registered: Include registered on-premises instances in the resulting list.
lopiRegistrationStatus :: Lens' ListOnPremisesInstances (Maybe RegistrationStatus)
lopiRegistrationStatus = lens _lopiRegistrationStatus (\ s a -> s{_lopiRegistrationStatus = a})

instance AWSRequest ListOnPremisesInstances where
        type Rs ListOnPremisesInstances =
             ListOnPremisesInstancesResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListOnPremisesInstancesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "instanceNames" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOnPremisesInstances where

instance NFData ListOnPremisesInstances where

instance ToHeaders ListOnPremisesInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListOnPremisesInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOnPremisesInstances where
        toJSON ListOnPremisesInstances'{..}
          = object
              (catMaybes
                 [("tagFilters" .=) <$> _lopiTagFilters,
                  ("nextToken" .=) <$> _lopiNextToken,
                  ("registrationStatus" .=) <$>
                    _lopiRegistrationStatus])

instance ToPath ListOnPremisesInstances where
        toPath = const "/"

instance ToQuery ListOnPremisesInstances where
        toQuery = const mempty

-- | Represents the output of list on-premises instances operation.
--
--
--
-- /See:/ 'listOnPremisesInstancesResponse' smart constructor.
data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse'
  { _lopirsNextToken      :: !(Maybe Text)
  , _lopirsInstanceNames  :: !(Maybe [Text])
  , _lopirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOnPremisesInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopirsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list on-premises instances call to return the next set of on-premises instances in the list.
--
-- * 'lopirsInstanceNames' - The list of matching on-premises instance names.
--
-- * 'lopirsResponseStatus' - -- | The response status code.
listOnPremisesInstancesResponse
    :: Int -- ^ 'lopirsResponseStatus'
    -> ListOnPremisesInstancesResponse
listOnPremisesInstancesResponse pResponseStatus_ =
  ListOnPremisesInstancesResponse'
    { _lopirsNextToken = Nothing
    , _lopirsInstanceNames = Nothing
    , _lopirsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list on-premises instances call to return the next set of on-premises instances in the list.
lopirsNextToken :: Lens' ListOnPremisesInstancesResponse (Maybe Text)
lopirsNextToken = lens _lopirsNextToken (\ s a -> s{_lopirsNextToken = a})

-- | The list of matching on-premises instance names.
lopirsInstanceNames :: Lens' ListOnPremisesInstancesResponse [Text]
lopirsInstanceNames = lens _lopirsInstanceNames (\ s a -> s{_lopirsInstanceNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lopirsResponseStatus :: Lens' ListOnPremisesInstancesResponse Int
lopirsResponseStatus = lens _lopirsResponseStatus (\ s a -> s{_lopirsResponseStatus = a})

instance NFData ListOnPremisesInstancesResponse where
