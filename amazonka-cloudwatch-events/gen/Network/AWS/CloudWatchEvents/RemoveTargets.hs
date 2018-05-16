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
-- Module      : Network.AWS.CloudWatchEvents.RemoveTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.
--
--
-- When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Please allow a short period of time for changes to take effect.
--
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is non-zero in the response and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
--
module Network.AWS.CloudWatchEvents.RemoveTargets
    (
    -- * Creating a Request
      removeTargets
    , RemoveTargets
    -- * Request Lenses
    , rtRule
    , rtIds

    -- * Destructuring the Response
    , removeTargetsResponse
    , RemoveTargetsResponse
    -- * Response Lenses
    , rtrsFailedEntryCount
    , rtrsFailedEntries
    , rtrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeTargets' smart constructor.
data RemoveTargets = RemoveTargets'
  { _rtRule :: !Text
  , _rtIds  :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtRule' - The name of the rule.
--
-- * 'rtIds' - The IDs of the targets to remove from the rule.
removeTargets
    :: Text -- ^ 'rtRule'
    -> NonEmpty Text -- ^ 'rtIds'
    -> RemoveTargets
removeTargets pRule_ pIds_ =
  RemoveTargets' {_rtRule = pRule_, _rtIds = _List1 # pIds_}


-- | The name of the rule.
rtRule :: Lens' RemoveTargets Text
rtRule = lens _rtRule (\ s a -> s{_rtRule = a})

-- | The IDs of the targets to remove from the rule.
rtIds :: Lens' RemoveTargets (NonEmpty Text)
rtIds = lens _rtIds (\ s a -> s{_rtIds = a}) . _List1

instance AWSRequest RemoveTargets where
        type Rs RemoveTargets = RemoveTargetsResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 RemoveTargetsResponse' <$>
                   (x .?> "FailedEntryCount") <*>
                     (x .?> "FailedEntries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable RemoveTargets where

instance NFData RemoveTargets where

instance ToHeaders RemoveTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.RemoveTargets" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTargets where
        toJSON RemoveTargets'{..}
          = object
              (catMaybes
                 [Just ("Rule" .= _rtRule), Just ("Ids" .= _rtIds)])

instance ToPath RemoveTargets where
        toPath = const "/"

instance ToQuery RemoveTargets where
        toQuery = const mempty

-- | /See:/ 'removeTargetsResponse' smart constructor.
data RemoveTargetsResponse = RemoveTargetsResponse'
  { _rtrsFailedEntryCount :: !(Maybe Int)
  , _rtrsFailedEntries    :: !(Maybe [RemoveTargetsResultEntry])
  , _rtrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsFailedEntryCount' - The number of failed entries.
--
-- * 'rtrsFailedEntries' - The failed target entries.
--
-- * 'rtrsResponseStatus' - -- | The response status code.
removeTargetsResponse
    :: Int -- ^ 'rtrsResponseStatus'
    -> RemoveTargetsResponse
removeTargetsResponse pResponseStatus_ =
  RemoveTargetsResponse'
    { _rtrsFailedEntryCount = Nothing
    , _rtrsFailedEntries = Nothing
    , _rtrsResponseStatus = pResponseStatus_
    }


-- | The number of failed entries.
rtrsFailedEntryCount :: Lens' RemoveTargetsResponse (Maybe Int)
rtrsFailedEntryCount = lens _rtrsFailedEntryCount (\ s a -> s{_rtrsFailedEntryCount = a})

-- | The failed target entries.
rtrsFailedEntries :: Lens' RemoveTargetsResponse [RemoveTargetsResultEntry]
rtrsFailedEntries = lens _rtrsFailedEntries (\ s a -> s{_rtrsFailedEntries = a}) . _Default . _Coerce

-- | -- | The response status code.
rtrsResponseStatus :: Lens' RemoveTargetsResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\ s a -> s{_rtrsResponseStatus = a})

instance NFData RemoveTargetsResponse where
