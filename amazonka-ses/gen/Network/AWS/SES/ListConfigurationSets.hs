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
-- Module      : Network.AWS.SES.ListConfigurationSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the configuration sets associated with your Amazon SES account. For information about using configuration sets, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity> in the /Amazon SES Developer Guide./
--
--
-- You can execute this operation no more than once per second. This operation will return up to 1,000 configuration sets each time it is run. If your Amazon SES account has more than 1,000 configuration sets, this operation will also return a NextToken element. You can then execute the @ListConfigurationSets@ operation again, passing the @NextToken@ parameter and the value of the NextToken element to retrieve additional results.
--
module Network.AWS.SES.ListConfigurationSets
    (
    -- * Creating a Request
      listConfigurationSets
    , ListConfigurationSets
    -- * Request Lenses
    , lcsNextToken
    , lcsMaxItems

    -- * Destructuring the Response
    , listConfigurationSetsResponse
    , ListConfigurationSetsResponse
    -- * Response Lenses
    , lcsrsConfigurationSets
    , lcsrsNextToken
    , lcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to list the configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'listConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { _lcsNextToken :: !(Maybe Text)
  , _lcsMaxItems  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurationSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsNextToken' - A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
--
-- * 'lcsMaxItems' - The number of configuration sets to return.
listConfigurationSets
    :: ListConfigurationSets
listConfigurationSets =
  ListConfigurationSets' {_lcsNextToken = Nothing, _lcsMaxItems = Nothing}


-- | A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
lcsNextToken :: Lens' ListConfigurationSets (Maybe Text)
lcsNextToken = lens _lcsNextToken (\ s a -> s{_lcsNextToken = a})

-- | The number of configuration sets to return.
lcsMaxItems :: Lens' ListConfigurationSets (Maybe Int)
lcsMaxItems = lens _lcsMaxItems (\ s a -> s{_lcsMaxItems = a})

instance AWSRequest ListConfigurationSets where
        type Rs ListConfigurationSets =
             ListConfigurationSetsResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "ListConfigurationSetsResult"
              (\ s h x ->
                 ListConfigurationSetsResponse' <$>
                   (x .@? "ConfigurationSets" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListConfigurationSets where

instance NFData ListConfigurationSets where

instance ToHeaders ListConfigurationSets where
        toHeaders = const mempty

instance ToPath ListConfigurationSets where
        toPath = const "/"

instance ToQuery ListConfigurationSets where
        toQuery ListConfigurationSets'{..}
          = mconcat
              ["Action" =: ("ListConfigurationSets" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "NextToken" =: _lcsNextToken,
               "MaxItems" =: _lcsMaxItems]

-- | A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'listConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { _lcsrsConfigurationSets :: !(Maybe [ConfigurationSet])
  , _lcsrsNextToken         :: !(Maybe Text)
  , _lcsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurationSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsrsConfigurationSets' - A list of configuration sets.
--
-- * 'lcsrsNextToken' - A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ .
--
-- * 'lcsrsResponseStatus' - -- | The response status code.
listConfigurationSetsResponse
    :: Int -- ^ 'lcsrsResponseStatus'
    -> ListConfigurationSetsResponse
listConfigurationSetsResponse pResponseStatus_ =
  ListConfigurationSetsResponse'
    { _lcsrsConfigurationSets = Nothing
    , _lcsrsNextToken = Nothing
    , _lcsrsResponseStatus = pResponseStatus_
    }


-- | A list of configuration sets.
lcsrsConfigurationSets :: Lens' ListConfigurationSetsResponse [ConfigurationSet]
lcsrsConfigurationSets = lens _lcsrsConfigurationSets (\ s a -> s{_lcsrsConfigurationSets = a}) . _Default . _Coerce

-- | A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ .
lcsrsNextToken :: Lens' ListConfigurationSetsResponse (Maybe Text)
lcsrsNextToken = lens _lcsrsNextToken (\ s a -> s{_lcsrsNextToken = a})

-- | -- | The response status code.
lcsrsResponseStatus :: Lens' ListConfigurationSetsResponse Int
lcsrsResponseStatus = lens _lcsrsResponseStatus (\ s a -> s{_lcsrsResponseStatus = a})

instance NFData ListConfigurationSetsResponse where
