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
-- Module      : Network.AWS.CloudWatchEvents.CreatePartnerEventSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Called by an SaaS partner to create a partner event source.
--
--
-- Each partner event source can be used by one AWS account to create a matching partner event bus in that AWS account. A SaaS partner must create one partner event source for each AWS account that wants to receive those event types.
--
-- A partner event source creates events based on resources in the SaaS partner's service or application.
--
-- An AWS account that creates a partner event bus that matches the partner event source can use that event bus to receive events from the partner, and then process them using AWS Events rules and targets.
--
-- Partner event source names follow this format:
--
-- @aws.partner//partner_name/ //event_namespace/ //event_name/ @
--
--     * /partner_name/ is determined during partner registration and identifies the partner to AWS customers.
--
--     * For /event_namespace/ , we recommend that partners use a string that identifies the AWS customer within the partner's system. This should not be the customer's AWS account ID.
--
--     * /event_name/ is determined by the partner, and should uniquely identify an event-generating resource within the partner system. This should help AWS customers decide whether to create an event bus to receive these events.
--
--
--
module Network.AWS.CloudWatchEvents.CreatePartnerEventSource
    (
    -- * Creating a Request
      createPartnerEventSource
    , CreatePartnerEventSource
    -- * Request Lenses
    , cpesName
    , cpesAccount

    -- * Destructuring the Response
    , createPartnerEventSourceResponse
    , CreatePartnerEventSourceResponse
    -- * Response Lenses
    , cpesrsEventSourceARN
    , cpesrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPartnerEventSource' smart constructor.
data CreatePartnerEventSource = CreatePartnerEventSource'
  { _cpesName    :: !Text
  , _cpesAccount :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePartnerEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpesName' - The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
--
-- * 'cpesAccount' - The AWS account ID of the customer who is permitted to create a matching partner event bus for this partner event source.
createPartnerEventSource
    :: Text -- ^ 'cpesName'
    -> Text -- ^ 'cpesAccount'
    -> CreatePartnerEventSource
createPartnerEventSource pName_ pAccount_ =
  CreatePartnerEventSource' {_cpesName = pName_, _cpesAccount = pAccount_}


-- | The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
cpesName :: Lens' CreatePartnerEventSource Text
cpesName = lens _cpesName (\ s a -> s{_cpesName = a})

-- | The AWS account ID of the customer who is permitted to create a matching partner event bus for this partner event source.
cpesAccount :: Lens' CreatePartnerEventSource Text
cpesAccount = lens _cpesAccount (\ s a -> s{_cpesAccount = a})

instance AWSRequest CreatePartnerEventSource where
        type Rs CreatePartnerEventSource =
             CreatePartnerEventSourceResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 CreatePartnerEventSourceResponse' <$>
                   (x .?> "EventSourceArn") <*> (pure (fromEnum s)))

instance Hashable CreatePartnerEventSource where

instance NFData CreatePartnerEventSource where

instance ToHeaders CreatePartnerEventSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.CreatePartnerEventSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePartnerEventSource where
        toJSON CreatePartnerEventSource'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cpesName),
                  Just ("Account" .= _cpesAccount)])

instance ToPath CreatePartnerEventSource where
        toPath = const "/"

instance ToQuery CreatePartnerEventSource where
        toQuery = const mempty

-- | /See:/ 'createPartnerEventSourceResponse' smart constructor.
data CreatePartnerEventSourceResponse = CreatePartnerEventSourceResponse'
  { _cpesrsEventSourceARN :: !(Maybe Text)
  , _cpesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePartnerEventSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpesrsEventSourceARN' - The ARN of the partner event source.
--
-- * 'cpesrsResponseStatus' - -- | The response status code.
createPartnerEventSourceResponse
    :: Int -- ^ 'cpesrsResponseStatus'
    -> CreatePartnerEventSourceResponse
createPartnerEventSourceResponse pResponseStatus_ =
  CreatePartnerEventSourceResponse'
    {_cpesrsEventSourceARN = Nothing, _cpesrsResponseStatus = pResponseStatus_}


-- | The ARN of the partner event source.
cpesrsEventSourceARN :: Lens' CreatePartnerEventSourceResponse (Maybe Text)
cpesrsEventSourceARN = lens _cpesrsEventSourceARN (\ s a -> s{_cpesrsEventSourceARN = a})

-- | -- | The response status code.
cpesrsResponseStatus :: Lens' CreatePartnerEventSourceResponse Int
cpesrsResponseStatus = lens _cpesrsResponseStatus (\ s a -> s{_cpesrsResponseStatus = a})

instance NFData CreatePartnerEventSourceResponse
         where
