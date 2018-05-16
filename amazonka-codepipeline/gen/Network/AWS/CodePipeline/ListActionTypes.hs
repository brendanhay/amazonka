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
-- Module      : Network.AWS.CodePipeline.ListActionTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all AWS CodePipeline action types associated with your account.
--
--
module Network.AWS.CodePipeline.ListActionTypes
    (
    -- * Creating a Request
      listActionTypes
    , ListActionTypes
    -- * Request Lenses
    , latActionOwnerFilter
    , latNextToken

    -- * Destructuring the Response
    , listActionTypesResponse
    , ListActionTypesResponse
    -- * Response Lenses
    , latrsNextToken
    , latrsResponseStatus
    , latrsActionTypes
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListActionTypes action.
--
--
--
-- /See:/ 'listActionTypes' smart constructor.
data ListActionTypes = ListActionTypes'
  { _latActionOwnerFilter :: !(Maybe ActionOwner)
  , _latNextToken         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActionTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latActionOwnerFilter' - Filters the list of action types to those created by a specified entity.
--
-- * 'latNextToken' - An identifier that was returned from the previous list action types call, which can be used to return the next set of action types in the list.
listActionTypes
    :: ListActionTypes
listActionTypes =
  ListActionTypes' {_latActionOwnerFilter = Nothing, _latNextToken = Nothing}


-- | Filters the list of action types to those created by a specified entity.
latActionOwnerFilter :: Lens' ListActionTypes (Maybe ActionOwner)
latActionOwnerFilter = lens _latActionOwnerFilter (\ s a -> s{_latActionOwnerFilter = a})

-- | An identifier that was returned from the previous list action types call, which can be used to return the next set of action types in the list.
latNextToken :: Lens' ListActionTypes (Maybe Text)
latNextToken = lens _latNextToken (\ s a -> s{_latNextToken = a})

instance AWSRequest ListActionTypes where
        type Rs ListActionTypes = ListActionTypesResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 ListActionTypesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "actionTypes" .!@ mempty))

instance Hashable ListActionTypes where

instance NFData ListActionTypes where

instance ToHeaders ListActionTypes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.ListActionTypes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListActionTypes where
        toJSON ListActionTypes'{..}
          = object
              (catMaybes
                 [("actionOwnerFilter" .=) <$> _latActionOwnerFilter,
                  ("nextToken" .=) <$> _latNextToken])

instance ToPath ListActionTypes where
        toPath = const "/"

instance ToQuery ListActionTypes where
        toQuery = const mempty

-- | Represents the output of a ListActionTypes action.
--
--
--
-- /See:/ 'listActionTypesResponse' smart constructor.
data ListActionTypesResponse = ListActionTypesResponse'
  { _latrsNextToken      :: !(Maybe Text)
  , _latrsResponseStatus :: !Int
  , _latrsActionTypes    :: ![ActionType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActionTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latrsNextToken' - If the amount of returned information is significantly large, an identifier is also returned which can be used in a subsequent list action types call to return the next set of action types in the list.
--
-- * 'latrsResponseStatus' - -- | The response status code.
--
-- * 'latrsActionTypes' - Provides details of the action types.
listActionTypesResponse
    :: Int -- ^ 'latrsResponseStatus'
    -> ListActionTypesResponse
listActionTypesResponse pResponseStatus_ =
  ListActionTypesResponse'
    { _latrsNextToken = Nothing
    , _latrsResponseStatus = pResponseStatus_
    , _latrsActionTypes = mempty
    }


-- | If the amount of returned information is significantly large, an identifier is also returned which can be used in a subsequent list action types call to return the next set of action types in the list.
latrsNextToken :: Lens' ListActionTypesResponse (Maybe Text)
latrsNextToken = lens _latrsNextToken (\ s a -> s{_latrsNextToken = a})

-- | -- | The response status code.
latrsResponseStatus :: Lens' ListActionTypesResponse Int
latrsResponseStatus = lens _latrsResponseStatus (\ s a -> s{_latrsResponseStatus = a})

-- | Provides details of the action types.
latrsActionTypes :: Lens' ListActionTypesResponse [ActionType]
latrsActionTypes = lens _latrsActionTypes (\ s a -> s{_latrsActionTypes = a}) . _Coerce

instance NFData ListActionTypesResponse where
