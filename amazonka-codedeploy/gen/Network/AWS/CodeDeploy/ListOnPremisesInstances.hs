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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of one or more on-premises instance names.
--
-- Unless otherwise specified, both registered and deregistered on-premises
-- instance names will be listed. To list only registered or deregistered
-- on-premises instance names, use the registration status parameter.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListOnPremisesInstances.html AWS API Reference> for ListOnPremisesInstances.
module Network.AWS.CodeDeploy.ListOnPremisesInstances
    (
    -- * Creating a Request
      ListOnPremisesInstances
    , listOnPremisesInstances
    -- * Request Lenses
    , lopiTagFilters
    , lopiNextToken
    , lopiRegistrationStatus

    -- * Destructuring the Response
    , ListOnPremisesInstancesResponse
    , listOnPremisesInstancesResponse
    -- * Response Lenses
    , lopirsNextToken
    , lopirsInstanceNames
    , lopirsStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
    { _lopiTagFilters :: !(Maybe [TagFilter])
    , _lopiNextToken :: !(Maybe Text)
    , _lopiRegistrationStatus :: !(Maybe RegistrationStatus)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
lopiTagFilters = lens _lopiTagFilters (\ s a -> s{_lopiTagFilters = a}) . _Default . _Coerce;

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
-- * 'lopirsNextToken'
--
-- * 'lopirsInstanceNames'
--
-- * 'lopirsStatus'
data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse'
    { _lopirsNextToken :: !(Maybe Text)
    , _lopirsInstanceNames :: !(Maybe [Text])
    , _lopirsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListOnPremisesInstancesResponse' smart constructor.
listOnPremisesInstancesResponse :: Int -> ListOnPremisesInstancesResponse
listOnPremisesInstancesResponse pStatus_ = 
    ListOnPremisesInstancesResponse'
    { _lopirsNextToken = Nothing
    , _lopirsInstanceNames = Nothing
    , _lopirsStatus = pStatus_
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- on-premises instances call to return the next set of on-premises
-- instances in the list.
lopirsNextToken :: Lens' ListOnPremisesInstancesResponse (Maybe Text)
lopirsNextToken = lens _lopirsNextToken (\ s a -> s{_lopirsNextToken = a});

-- | The list of matching on-premises instance names.
lopirsInstanceNames :: Lens' ListOnPremisesInstancesResponse [Text]
lopirsInstanceNames = lens _lopirsInstanceNames (\ s a -> s{_lopirsInstanceNames = a}) . _Default . _Coerce;

-- | Undocumented member.
lopirsStatus :: Lens' ListOnPremisesInstancesResponse Int
lopirsStatus = lens _lopirsStatus (\ s a -> s{_lopirsStatus = a});
