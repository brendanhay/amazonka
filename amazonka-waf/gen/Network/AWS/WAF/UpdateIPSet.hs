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
-- Module      : Network.AWS.WAF.UpdateIPSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes < IPSetDescriptor> objects in an 'IPSet'. For each 'IPSetDescriptor' object, you specify the following values:
--
-- -   Whether to insert or delete the object from the array. If you want to change an 'IPSetDescriptor' object, you delete the existing object and add a new one.
-- -   The IP address version, 'IPv4'.
-- -   The IP address in CIDR notation, for example, '192.0.2.0\/24' (for the range of IP addresses from '192.0.2.0' to '192.0.2.255') or '192.0.2.44\/32' (for the individual IP address '192.0.2.44').
--
-- AWS WAF supports \/8, \/16, \/24, and \/32 IP address ranges. For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- You use an 'IPSet' to specify which web requests you want to allow or block based on the IP addresses that the requests originated from. For example, if you\'re receiving a lot of requests from one or a small number of IP addresses and you want to block the requests, you can create an 'IPSet' that specifies those IP addresses, and then configure AWS WAF to block the requests.
--
-- To create and configure an 'IPSet', perform the following steps:
--
-- 1.  Submit a < CreateIPSet> request.
-- 2.  Use < GetChangeToken> to get the change token that you provide in the 'ChangeToken' parameter of an < UpdateIPSet> request.
-- 3.  Submit an 'UpdateIPSet' request to specify the IP addresses that you want AWS WAF to watch for.
--
-- When you update an 'IPSet', you specify the IP addresses that you want to add and\/or the IP addresses that you want to delete. If you want to change an IP address, you delete the existing IP address and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.UpdateIPSet
    (
    -- * Creating a Request
      updateIPSet
    , UpdateIPSet
    -- * Request Lenses
    , uisIPSetId
    , uisChangeToken
    , uisUpdates

    -- * Destructuring the Response
    , updateIPSetResponse
    , UpdateIPSetResponse
    -- * Response Lenses
    , uisrsChangeToken
    , uisrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WAF.Types
import           Network.AWS.WAF.Types.Product

-- | /See:/ 'updateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
    { _uisIPSetId     :: !Text
    , _uisChangeToken :: !Text
    , _uisUpdates     :: ![IPSetUpdate]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uisIPSetId'
--
-- * 'uisChangeToken'
--
-- * 'uisUpdates'
updateIPSet
    :: Text -- ^ 'uisIPSetId'
    -> Text -- ^ 'uisChangeToken'
    -> UpdateIPSet
updateIPSet pIPSetId_ pChangeToken_ =
    UpdateIPSet'
    { _uisIPSetId = pIPSetId_
    , _uisChangeToken = pChangeToken_
    , _uisUpdates = mempty
    }

-- | The 'IPSetId' of the < IPSet> that you want to update. 'IPSetId' is returned by < CreateIPSet> and by < ListIPSets>.
uisIPSetId :: Lens' UpdateIPSet Text
uisIPSetId = lens _uisIPSetId (\ s a -> s{_uisIPSetId = a});

-- | The value returned by the most recent call to < GetChangeToken>.
uisChangeToken :: Lens' UpdateIPSet Text
uisChangeToken = lens _uisChangeToken (\ s a -> s{_uisChangeToken = a});

-- | An array of 'IPSetUpdate' objects that you want to insert into or delete from an < IPSet>. For more information, see the applicable data types:
--
-- -   < IPSetUpdate>: Contains 'Action' and 'IPSetDescriptor'
-- -   < IPSetDescriptor>: Contains 'Type' and 'Value'
uisUpdates :: Lens' UpdateIPSet [IPSetUpdate]
uisUpdates = lens _uisUpdates (\ s a -> s{_uisUpdates = a}) . _Coerce;

instance AWSRequest UpdateIPSet where
        type Rs UpdateIPSet = UpdateIPSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateIPSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateIPSet

instance NFData UpdateIPSet

instance ToHeaders UpdateIPSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateIPSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateIPSet where
        toJSON UpdateIPSet'{..}
          = object
              (catMaybes
                 [Just ("IPSetId" .= _uisIPSetId),
                  Just ("ChangeToken" .= _uisChangeToken),
                  Just ("Updates" .= _uisUpdates)])

instance ToPath UpdateIPSet where
        toPath = const "/"

instance ToQuery UpdateIPSet where
        toQuery = const mempty

-- | /See:/ 'updateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
    { _uisrsChangeToken    :: !(Maybe Text)
    , _uisrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uisrsChangeToken'
--
-- * 'uisrsResponseStatus'
updateIPSetResponse
    :: Int -- ^ 'uisrsResponseStatus'
    -> UpdateIPSetResponse
updateIPSetResponse pResponseStatus_ =
    UpdateIPSetResponse'
    { _uisrsChangeToken = Nothing
    , _uisrsResponseStatus = pResponseStatus_
    }

-- | The 'ChangeToken' that you used to submit the 'UpdateIPSet' request. You can also use this value to query the status of the request. For more information, see < GetChangeTokenStatus>.
uisrsChangeToken :: Lens' UpdateIPSetResponse (Maybe Text)
uisrsChangeToken = lens _uisrsChangeToken (\ s a -> s{_uisrsChangeToken = a});

-- | The response status code.
uisrsResponseStatus :: Lens' UpdateIPSetResponse Int
uisrsResponseStatus = lens _uisrsResponseStatus (\ s a -> s{_uisrsResponseStatus = a});

instance NFData UpdateIPSetResponse
