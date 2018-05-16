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
-- Module      : Network.AWS.IoT.ListV2LoggingLevels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists logging levels.
--
--
module Network.AWS.IoT.ListV2LoggingLevels
    (
    -- * Creating a Request
      listV2LoggingLevels
    , ListV2LoggingLevels
    -- * Request Lenses
    , lvllTargetType
    , lvllNextToken
    , lvllMaxResults

    -- * Destructuring the Response
    , listV2LoggingLevelsResponse
    , ListV2LoggingLevelsResponse
    -- * Response Lenses
    , lvllrsLogTargetConfigurations
    , lvllrsNextToken
    , lvllrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listV2LoggingLevels' smart constructor.
data ListV2LoggingLevels = ListV2LoggingLevels'
  { _lvllTargetType :: !(Maybe LogTargetType)
  , _lvllNextToken  :: !(Maybe Text)
  , _lvllMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListV2LoggingLevels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvllTargetType' - The type of resource for which you are configuring logging. Must be @THING_Group@ .
--
-- * 'lvllNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'lvllMaxResults' - The maximum number of results to return at one time.
listV2LoggingLevels
    :: ListV2LoggingLevels
listV2LoggingLevels =
  ListV2LoggingLevels'
    { _lvllTargetType = Nothing
    , _lvllNextToken = Nothing
    , _lvllMaxResults = Nothing
    }


-- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
lvllTargetType :: Lens' ListV2LoggingLevels (Maybe LogTargetType)
lvllTargetType = lens _lvllTargetType (\ s a -> s{_lvllTargetType = a})

-- | The token used to get the next set of results, or __null__ if there are no additional results.
lvllNextToken :: Lens' ListV2LoggingLevels (Maybe Text)
lvllNextToken = lens _lvllNextToken (\ s a -> s{_lvllNextToken = a})

-- | The maximum number of results to return at one time.
lvllMaxResults :: Lens' ListV2LoggingLevels (Maybe Natural)
lvllMaxResults = lens _lvllMaxResults (\ s a -> s{_lvllMaxResults = a}) . mapping _Nat

instance AWSRequest ListV2LoggingLevels where
        type Rs ListV2LoggingLevels =
             ListV2LoggingLevelsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListV2LoggingLevelsResponse' <$>
                   (x .?> "logTargetConfigurations" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListV2LoggingLevels where

instance NFData ListV2LoggingLevels where

instance ToHeaders ListV2LoggingLevels where
        toHeaders = const mempty

instance ToPath ListV2LoggingLevels where
        toPath = const "/v2LoggingLevel"

instance ToQuery ListV2LoggingLevels where
        toQuery ListV2LoggingLevels'{..}
          = mconcat
              ["targetType" =: _lvllTargetType,
               "nextToken" =: _lvllNextToken,
               "maxResults" =: _lvllMaxResults]

-- | /See:/ 'listV2LoggingLevelsResponse' smart constructor.
data ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse'
  { _lvllrsLogTargetConfigurations :: !(Maybe [LogTargetConfiguration])
  , _lvllrsNextToken               :: !(Maybe Text)
  , _lvllrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListV2LoggingLevelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvllrsLogTargetConfigurations' - The logging configuration for a target.
--
-- * 'lvllrsNextToken' - The token used to get the next set of results, or __null__ if there are no additional results.
--
-- * 'lvllrsResponseStatus' - -- | The response status code.
listV2LoggingLevelsResponse
    :: Int -- ^ 'lvllrsResponseStatus'
    -> ListV2LoggingLevelsResponse
listV2LoggingLevelsResponse pResponseStatus_ =
  ListV2LoggingLevelsResponse'
    { _lvllrsLogTargetConfigurations = Nothing
    , _lvllrsNextToken = Nothing
    , _lvllrsResponseStatus = pResponseStatus_
    }


-- | The logging configuration for a target.
lvllrsLogTargetConfigurations :: Lens' ListV2LoggingLevelsResponse [LogTargetConfiguration]
lvllrsLogTargetConfigurations = lens _lvllrsLogTargetConfigurations (\ s a -> s{_lvllrsLogTargetConfigurations = a}) . _Default . _Coerce

-- | The token used to get the next set of results, or __null__ if there are no additional results.
lvllrsNextToken :: Lens' ListV2LoggingLevelsResponse (Maybe Text)
lvllrsNextToken = lens _lvllrsNextToken (\ s a -> s{_lvllrsNextToken = a})

-- | -- | The response status code.
lvllrsResponseStatus :: Lens' ListV2LoggingLevelsResponse Int
lvllrsResponseStatus = lens _lvllrsResponseStatus (\ s a -> s{_lvllrsResponseStatus = a})

instance NFData ListV2LoggingLevelsResponse where
