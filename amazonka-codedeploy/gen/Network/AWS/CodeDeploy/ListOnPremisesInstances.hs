{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CodeDeploy.ListOnPremisesInstances
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets a list of one or more on-premises instance names.
--
-- Unless otherwise specified, both registered and deregistered on-premises
-- instance names will be listed. To list only registered or deregistered
-- on-premises instance names, use the registration status parameter.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListOnPremisesInstances.html>
module Network.AWS.CodeDeploy.ListOnPremisesInstances
    (
    -- * Request
      ListOnPremisesInstances
    -- ** Request constructor
    , listOnPremisesInstances
    -- ** Request lenses
    , lopiTagFilters
    , lopiNextToken
    , lopiRegistrationStatus

    -- * Response
    , ListOnPremisesInstancesResponse
    -- ** Response constructor
    , listOnPremisesInstancesResponse
    -- ** Response lenses
    , lopirNextToken
    , lopirInstanceNames
    , lopirStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list on-premises instances operation.
--
-- .
--
-- /See:/ 'listOnPremisesInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lopiTagFilters'
--
-- * 'lopiNextToken'
--
-- * 'lopiRegistrationStatus'
data ListOnPremisesInstances = ListOnPremisesInstances'
    { _lopiTagFilters         :: Maybe [TagFilter]
    , _lopiNextToken          :: Maybe Text
    , _lopiRegistrationStatus :: Maybe RegistrationStatus
    } deriving (Eq,Read,Show)

-- | 'ListOnPremisesInstances' smart constructor.
listOnPremisesInstances :: ListOnPremisesInstances
listOnPremisesInstances =
    ListOnPremisesInstances'
    { _lopiTagFilters = Nothing
    , _lopiNextToken = Nothing
    , _lopiRegistrationStatus = Nothing
    }

-- | The on-premises instance tags that will be used to restrict the
-- corresponding on-premises instance names that are returned.
lopiTagFilters :: Lens' ListOnPremisesInstances [TagFilter]
lopiTagFilters = lens _lopiTagFilters (\ s a -> s{_lopiTagFilters = a}) . _Default;

-- | An identifier that was returned from the previous list on-premises
-- instances call, which can be used to return the next set of on-premises
-- instances in the list.
lopiNextToken :: Lens' ListOnPremisesInstances (Maybe Text)
lopiNextToken = lens _lopiNextToken (\ s a -> s{_lopiNextToken = a});

-- | The on-premises instances registration status:
--
-- -   Deregistered: Include in the resulting list deregistered on-premises
--     instances.
-- -   Registered: Include in the resulting list registered on-premises
--     instances.
lopiRegistrationStatus :: Lens' ListOnPremisesInstances (Maybe RegistrationStatus)
lopiRegistrationStatus = lens _lopiRegistrationStatus (\ s a -> s{_lopiRegistrationStatus = a});

instance AWSRequest ListOnPremisesInstances where
        type Sv ListOnPremisesInstances = CodeDeploy
        type Rs ListOnPremisesInstances =
             ListOnPremisesInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListOnPremisesInstancesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "instanceNames" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["tagFilters" .= _lopiTagFilters,
               "nextToken" .= _lopiNextToken,
               "registrationStatus" .= _lopiRegistrationStatus]

instance ToPath ListOnPremisesInstances where
        toPath = const "/"

instance ToQuery ListOnPremisesInstances where
        toQuery = const mempty

-- | Represents the output of list on-premises instances operation.
--
-- /See:/ 'listOnPremisesInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lopirNextToken'
--
-- * 'lopirInstanceNames'
--
-- * 'lopirStatus'
data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse'
    { _lopirNextToken     :: Maybe Text
    , _lopirInstanceNames :: Maybe [Text]
    , _lopirStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListOnPremisesInstancesResponse' smart constructor.
listOnPremisesInstancesResponse :: Int -> ListOnPremisesInstancesResponse
listOnPremisesInstancesResponse pStatus =
    ListOnPremisesInstancesResponse'
    { _lopirNextToken = Nothing
    , _lopirInstanceNames = Nothing
    , _lopirStatus = pStatus
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- on-premises instances call to return the next set of on-premises
-- instances in the list.
lopirNextToken :: Lens' ListOnPremisesInstancesResponse (Maybe Text)
lopirNextToken = lens _lopirNextToken (\ s a -> s{_lopirNextToken = a});

-- | The list of matching on-premises instance names.
lopirInstanceNames :: Lens' ListOnPremisesInstancesResponse [Text]
lopirInstanceNames = lens _lopirInstanceNames (\ s a -> s{_lopirInstanceNames = a}) . _Default;

-- | FIXME: Undocumented member.
lopirStatus :: Lens' ListOnPremisesInstancesResponse Int
lopirStatus = lens _lopirStatus (\ s a -> s{_lopirStatus = a});
