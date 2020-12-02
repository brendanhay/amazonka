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
-- Module      : Network.AWS.DMS.ModifyEventSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing AWS DMS event notification subscription.
--
--
module Network.AWS.DMS.ModifyEventSubscription
    (
    -- * Creating a Request
      modifyEventSubscription
    , ModifyEventSubscription
    -- * Request Lenses
    , mesSNSTopicARN
    , mesEnabled
    , mesSourceType
    , mesEventCategories
    , mesSubscriptionName

    -- * Destructuring the Response
    , modifyEventSubscriptionResponse
    , ModifyEventSubscriptionResponse
    -- * Response Lenses
    , mesrsEventSubscription
    , mesrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyEventSubscription' smart constructor.
data ModifyEventSubscription = ModifyEventSubscription'
  { _mesSNSTopicARN      :: !(Maybe Text)
  , _mesEnabled          :: !(Maybe Bool)
  , _mesSourceType       :: !(Maybe Text)
  , _mesEventCategories  :: !(Maybe [Text])
  , _mesSubscriptionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mesSNSTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
--
-- * 'mesEnabled' - A Boolean value; set to __true__ to activate the subscription.
--
-- * 'mesSourceType' - The type of AWS DMS resource that generates the events you want to subscribe to.  Valid values: replication-instance | migration-task
--
-- * 'mesEventCategories' - A list of event categories for a source type that you want to subscribe to. Use the @DescribeEventCategories@ action to see a list of event categories.
--
-- * 'mesSubscriptionName' - The name of the AWS DMS event notification subscription to be modified.
modifyEventSubscription
    :: Text -- ^ 'mesSubscriptionName'
    -> ModifyEventSubscription
modifyEventSubscription pSubscriptionName_ =
  ModifyEventSubscription'
    { _mesSNSTopicARN = Nothing
    , _mesEnabled = Nothing
    , _mesSourceType = Nothing
    , _mesEventCategories = Nothing
    , _mesSubscriptionName = pSubscriptionName_
    }


-- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
mesSNSTopicARN :: Lens' ModifyEventSubscription (Maybe Text)
mesSNSTopicARN = lens _mesSNSTopicARN (\ s a -> s{_mesSNSTopicARN = a})

-- | A Boolean value; set to __true__ to activate the subscription.
mesEnabled :: Lens' ModifyEventSubscription (Maybe Bool)
mesEnabled = lens _mesEnabled (\ s a -> s{_mesEnabled = a})

-- | The type of AWS DMS resource that generates the events you want to subscribe to.  Valid values: replication-instance | migration-task
mesSourceType :: Lens' ModifyEventSubscription (Maybe Text)
mesSourceType = lens _mesSourceType (\ s a -> s{_mesSourceType = a})

-- | A list of event categories for a source type that you want to subscribe to. Use the @DescribeEventCategories@ action to see a list of event categories.
mesEventCategories :: Lens' ModifyEventSubscription [Text]
mesEventCategories = lens _mesEventCategories (\ s a -> s{_mesEventCategories = a}) . _Default . _Coerce

-- | The name of the AWS DMS event notification subscription to be modified.
mesSubscriptionName :: Lens' ModifyEventSubscription Text
mesSubscriptionName = lens _mesSubscriptionName (\ s a -> s{_mesSubscriptionName = a})

instance AWSRequest ModifyEventSubscription where
        type Rs ModifyEventSubscription =
             ModifyEventSubscriptionResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ModifyEventSubscriptionResponse' <$>
                   (x .?> "EventSubscription") <*> (pure (fromEnum s)))

instance Hashable ModifyEventSubscription where

instance NFData ModifyEventSubscription where

instance ToHeaders ModifyEventSubscription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ModifyEventSubscription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyEventSubscription where
        toJSON ModifyEventSubscription'{..}
          = object
              (catMaybes
                 [("SnsTopicArn" .=) <$> _mesSNSTopicARN,
                  ("Enabled" .=) <$> _mesEnabled,
                  ("SourceType" .=) <$> _mesSourceType,
                  ("EventCategories" .=) <$> _mesEventCategories,
                  Just ("SubscriptionName" .= _mesSubscriptionName)])

instance ToPath ModifyEventSubscription where
        toPath = const "/"

instance ToQuery ModifyEventSubscription where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'modifyEventSubscriptionResponse' smart constructor.
data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse'
  { _mesrsEventSubscription :: !(Maybe EventSubscription)
  , _mesrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mesrsEventSubscription' - The modified event subscription.
--
-- * 'mesrsResponseStatus' - -- | The response status code.
modifyEventSubscriptionResponse
    :: Int -- ^ 'mesrsResponseStatus'
    -> ModifyEventSubscriptionResponse
modifyEventSubscriptionResponse pResponseStatus_ =
  ModifyEventSubscriptionResponse'
    {_mesrsEventSubscription = Nothing, _mesrsResponseStatus = pResponseStatus_}


-- | The modified event subscription.
mesrsEventSubscription :: Lens' ModifyEventSubscriptionResponse (Maybe EventSubscription)
mesrsEventSubscription = lens _mesrsEventSubscription (\ s a -> s{_mesrsEventSubscription = a})

-- | -- | The response status code.
mesrsResponseStatus :: Lens' ModifyEventSubscriptionResponse Int
mesrsResponseStatus = lens _mesrsResponseStatus (\ s a -> s{_mesrsResponseStatus = a})

instance NFData ModifyEventSubscriptionResponse where
