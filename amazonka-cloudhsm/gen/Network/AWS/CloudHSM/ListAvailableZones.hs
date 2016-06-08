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
-- Module      : Network.AWS.CloudHSM.ListAvailableZones
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Availability Zones that have available AWS CloudHSM capacity.
module Network.AWS.CloudHSM.ListAvailableZones
    (
    -- * Creating a Request
      listAvailableZones
    , ListAvailableZones

    -- * Destructuring the Response
    , listAvailableZonesResponse
    , ListAvailableZonesResponse
    -- * Response Lenses
    , lazrsAZList
    , lazrsResponseStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the < ListAvailableZones> action.
--
-- /See:/ 'listAvailableZones' smart constructor.
data ListAvailableZones =
    ListAvailableZones'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAvailableZones' with the minimum fields required to make a request.
--
listAvailableZones
    :: ListAvailableZones
listAvailableZones = ListAvailableZones'

instance AWSRequest ListAvailableZones where
        type Rs ListAvailableZones =
             ListAvailableZonesResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 ListAvailableZonesResponse' <$>
                   (x .?> "AZList" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable ListAvailableZones

instance NFData ListAvailableZones

instance ToHeaders ListAvailableZones where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListAvailableZones" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAvailableZones where
        toJSON = const (Object mempty)

instance ToPath ListAvailableZones where
        toPath = const "/"

instance ToQuery ListAvailableZones where
        toQuery = const mempty

-- | /See:/ 'listAvailableZonesResponse' smart constructor.
data ListAvailableZonesResponse = ListAvailableZonesResponse'
    { _lazrsAZList         :: !(Maybe [Text])
    , _lazrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAvailableZonesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lazrsAZList'
--
-- * 'lazrsResponseStatus'
listAvailableZonesResponse
    :: Int -- ^ 'lazrsResponseStatus'
    -> ListAvailableZonesResponse
listAvailableZonesResponse pResponseStatus_ =
    ListAvailableZonesResponse'
    { _lazrsAZList = Nothing
    , _lazrsResponseStatus = pResponseStatus_
    }

-- | The list of Availability Zones that have available AWS CloudHSM capacity.
lazrsAZList :: Lens' ListAvailableZonesResponse [Text]
lazrsAZList = lens _lazrsAZList (\ s a -> s{_lazrsAZList = a}) . _Default . _Coerce;

-- | The response status code.
lazrsResponseStatus :: Lens' ListAvailableZonesResponse Int
lazrsResponseStatus = lens _lazrsResponseStatus (\ s a -> s{_lazrsResponseStatus = a});

instance NFData ListAvailableZonesResponse
