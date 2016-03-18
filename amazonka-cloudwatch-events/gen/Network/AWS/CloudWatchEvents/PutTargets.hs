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
-- Module      : Network.AWS.CloudWatchEvents.PutTargets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds target(s) to a rule. Targets are the resources that can be invoked
-- when a rule is triggered. For example, AWS Lambda functions, Amazon
-- Kinesis streams, and built-in targets. Updates the target(s) if they are
-- already associated with the role. In other words, if there is already a
-- target with the given target ID, then the target associated with that ID
-- is updated.
--
-- In order to be able to make API calls against the resources you own,
-- Amazon CloudWatch Events needs the appropriate permissions. For AWS
-- Lambda and Amazon SNS resources, CloudWatch Events relies on
-- resource-based policies. For Amazon Kinesis streams, CloudWatch Events
-- relies on IAM roles. For more information, see
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/EventsTargetPermissions.html Permissions for Sending Events to Targets>
-- in the __/Amazon CloudWatch Developer Guide/__.
--
-- __Input__ and __InputPath__ are mutually-exclusive and optional
-- parameters of a target. When a rule is triggered due to a matched event,
-- if for a target:
--
-- -   Neither __Input__ nor __InputPath__ is specified, then the entire
--     event is passed to the target in JSON form.
-- -   __InputPath__ is specified in the form of JSONPath (e.g.
--     __$.detail__), then only the part of the event specified in the path
--     is passed to the target (e.g. only the detail part of the event is
--     passed).
-- -   __Input__ is specified in the form of a valid JSON, then the matched
--     event is overridden with this constant.
--
-- __Note:__ When you add targets to a rule, when the associated rule
-- triggers, new or updated targets might not be immediately invoked.
-- Please allow a short period of time for changes to take effect.
module Network.AWS.CloudWatchEvents.PutTargets
    (
    -- * Creating a Request
      putTargets
    , PutTargets
    -- * Request Lenses
    , ptRule
    , ptTargets

    -- * Destructuring the Response
    , putTargetsResponse
    , PutTargetsResponse
    -- * Response Lenses
    , ptrsFailedEntryCount
    , ptrsFailedEntries
    , ptrsResponseStatus
    ) where

import           Network.AWS.CloudWatchEvents.Types
import           Network.AWS.CloudWatchEvents.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the < PutTargets> operation.
--
-- /See:/ 'putTargets' smart constructor.
data PutTargets = PutTargets'
    { _ptRule    :: !Text
    , _ptTargets :: ![Target]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptRule'
--
-- * 'ptTargets'
putTargets
    :: Text -- ^ 'ptRule'
    -> PutTargets
putTargets pRule_ =
    PutTargets'
    { _ptRule = pRule_
    , _ptTargets = mempty
    }

-- | The name of the rule you want to add targets to.
ptRule :: Lens' PutTargets Text
ptRule = lens _ptRule (\ s a -> s{_ptRule = a});

-- | List of targets you want to update or add to the rule.
ptTargets :: Lens' PutTargets [Target]
ptTargets = lens _ptTargets (\ s a -> s{_ptTargets = a}) . _Coerce;

instance AWSRequest PutTargets where
        type Rs PutTargets = PutTargetsResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 PutTargetsResponse' <$>
                   (x .?> "FailedEntryCount") <*>
                     (x .?> "FailedEntries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders PutTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutTargets" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutTargets where
        toJSON PutTargets'{..}
          = object
              (catMaybes
                 [Just ("Rule" .= _ptRule),
                  Just ("Targets" .= _ptTargets)])

instance ToPath PutTargets where
        toPath = const "/"

instance ToQuery PutTargets where
        toQuery = const mempty

-- | The result of the < PutTargets> operation.
--
-- /See:/ 'putTargetsResponse' smart constructor.
data PutTargetsResponse = PutTargetsResponse'
    { _ptrsFailedEntryCount :: !(Maybe Int)
    , _ptrsFailedEntries    :: !(Maybe [PutTargetsResultEntry])
    , _ptrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrsFailedEntryCount'
--
-- * 'ptrsFailedEntries'
--
-- * 'ptrsResponseStatus'
putTargetsResponse
    :: Int -- ^ 'ptrsResponseStatus'
    -> PutTargetsResponse
putTargetsResponse pResponseStatus_ =
    PutTargetsResponse'
    { _ptrsFailedEntryCount = Nothing
    , _ptrsFailedEntries = Nothing
    , _ptrsResponseStatus = pResponseStatus_
    }

-- | The number of failed entries.
ptrsFailedEntryCount :: Lens' PutTargetsResponse (Maybe Int)
ptrsFailedEntryCount = lens _ptrsFailedEntryCount (\ s a -> s{_ptrsFailedEntryCount = a});

-- | An array of failed target entries.
ptrsFailedEntries :: Lens' PutTargetsResponse [PutTargetsResultEntry]
ptrsFailedEntries = lens _ptrsFailedEntries (\ s a -> s{_ptrsFailedEntries = a}) . _Default . _Coerce;

-- | The response status code.
ptrsResponseStatus :: Lens' PutTargetsResponse Int
ptrsResponseStatus = lens _ptrsResponseStatus (\ s a -> s{_ptrsResponseStatus = a});
