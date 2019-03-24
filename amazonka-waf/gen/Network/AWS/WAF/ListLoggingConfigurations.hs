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
-- Module      : Network.AWS.WAF.ListLoggingConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'LoggingConfiguration' objects.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListLoggingConfigurations
    (
    -- * Creating a Request
      listLoggingConfigurations
    , ListLoggingConfigurations
    -- * Request Lenses
    , llcNextMarker
    , llcLimit

    -- * Destructuring the Response
    , listLoggingConfigurationsResponse
    , ListLoggingConfigurationsResponse
    -- * Response Lenses
    , llcrsNextMarker
    , llcrsLoggingConfigurations
    , llcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'listLoggingConfigurations' smart constructor.
data ListLoggingConfigurations = ListLoggingConfigurations'
  { _llcNextMarker :: !(Maybe Text)
  , _llcLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLoggingConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llcNextMarker' - If you specify a value for @Limit@ and you have more @LoggingConfigurations@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @LoggingConfigurations@ . For the second and subsequent @ListLoggingConfigurations@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ListLoggingConfigurations@ .
--
-- * 'llcLimit' - Specifies the number of @LoggingConfigurations@ that you want AWS WAF to return for this request. If you have more @LoggingConfigurations@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @LoggingConfigurations@ .
listLoggingConfigurations
    :: ListLoggingConfigurations
listLoggingConfigurations =
  ListLoggingConfigurations' {_llcNextMarker = Nothing, _llcLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @LoggingConfigurations@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @LoggingConfigurations@ . For the second and subsequent @ListLoggingConfigurations@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ListLoggingConfigurations@ .
llcNextMarker :: Lens' ListLoggingConfigurations (Maybe Text)
llcNextMarker = lens _llcNextMarker (\ s a -> s{_llcNextMarker = a})

-- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to return for this request. If you have more @LoggingConfigurations@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @LoggingConfigurations@ .
llcLimit :: Lens' ListLoggingConfigurations (Maybe Natural)
llcLimit = lens _llcLimit (\ s a -> s{_llcLimit = a}) . mapping _Nat

instance AWSPager ListLoggingConfigurations where
        page rq rs
          | stop (rs ^. llcrsNextMarker) = Nothing
          | stop (rs ^. llcrsLoggingConfigurations) = Nothing
          | otherwise =
            Just $ rq & llcNextMarker .~ rs ^. llcrsNextMarker

instance AWSRequest ListLoggingConfigurations where
        type Rs ListLoggingConfigurations =
             ListLoggingConfigurationsResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 ListLoggingConfigurationsResponse' <$>
                   (x .?> "NextMarker") <*>
                     (x .?> "LoggingConfigurations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListLoggingConfigurations where

instance NFData ListLoggingConfigurations where

instance ToHeaders ListLoggingConfigurations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.ListLoggingConfigurations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListLoggingConfigurations where
        toJSON ListLoggingConfigurations'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _llcNextMarker,
                  ("Limit" .=) <$> _llcLimit])

instance ToPath ListLoggingConfigurations where
        toPath = const "/"

instance ToQuery ListLoggingConfigurations where
        toQuery = const mempty

-- | /See:/ 'listLoggingConfigurationsResponse' smart constructor.
data ListLoggingConfigurationsResponse = ListLoggingConfigurationsResponse'
  { _llcrsNextMarker            :: !(Maybe Text)
  , _llcrsLoggingConfigurations :: !(Maybe [LoggingConfiguration])
  , _llcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLoggingConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llcrsNextMarker' - If you have more @LoggingConfigurations@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @LoggingConfigurations@ , submit another @ListLoggingConfigurations@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'llcrsLoggingConfigurations' - An array of 'LoggingConfiguration' objects.
--
-- * 'llcrsResponseStatus' - -- | The response status code.
listLoggingConfigurationsResponse
    :: Int -- ^ 'llcrsResponseStatus'
    -> ListLoggingConfigurationsResponse
listLoggingConfigurationsResponse pResponseStatus_ =
  ListLoggingConfigurationsResponse'
    { _llcrsNextMarker = Nothing
    , _llcrsLoggingConfigurations = Nothing
    , _llcrsResponseStatus = pResponseStatus_
    }


-- | If you have more @LoggingConfigurations@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @LoggingConfigurations@ , submit another @ListLoggingConfigurations@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
llcrsNextMarker :: Lens' ListLoggingConfigurationsResponse (Maybe Text)
llcrsNextMarker = lens _llcrsNextMarker (\ s a -> s{_llcrsNextMarker = a})

-- | An array of 'LoggingConfiguration' objects.
llcrsLoggingConfigurations :: Lens' ListLoggingConfigurationsResponse [LoggingConfiguration]
llcrsLoggingConfigurations = lens _llcrsLoggingConfigurations (\ s a -> s{_llcrsLoggingConfigurations = a}) . _Default . _Coerce

-- | -- | The response status code.
llcrsResponseStatus :: Lens' ListLoggingConfigurationsResponse Int
llcrsResponseStatus = lens _llcrsResponseStatus (\ s a -> s{_llcrsResponseStatus = a})

instance NFData ListLoggingConfigurationsResponse
         where
