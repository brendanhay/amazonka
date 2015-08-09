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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all AWS CodePipeline action types associated with your
-- account.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_ListActionTypes.html AWS API Reference> for ListActionTypes.
module Network.AWS.CodePipeline.ListActionTypes
    (
    -- * Creating a Request
      ListActionTypes
    , listActionTypes
    -- * Request Lenses
    , latActionOwnerFilter
    , latNextToken

    -- * Destructuring the Response
    , ListActionTypesResponse
    , listActionTypesResponse
    -- * Response Lenses
    , latrsNextToken
    , latrsStatus
    , latrsActionTypes
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a list action types action.
--
-- /See:/ 'listActionTypes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'latActionOwnerFilter'
--
-- * 'latNextToken'
data ListActionTypes = ListActionTypes'
    { _latActionOwnerFilter :: !(Maybe ActionOwner)
    , _latNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListActionTypes' smart constructor.
listActionTypes :: ListActionTypes
listActionTypes = 
    ListActionTypes'
    { _latActionOwnerFilter = Nothing
    , _latNextToken = Nothing
    }

-- | Filters the list of action types to those created by a specified entity.
latActionOwnerFilter :: Lens' ListActionTypes (Maybe ActionOwner)
latActionOwnerFilter = lens _latActionOwnerFilter (\ s a -> s{_latActionOwnerFilter = a});

-- | An identifier that was returned from the previous list action types
-- call, which can be used to return the next set of action types in the
-- list.
latNextToken :: Lens' ListActionTypes (Maybe Text)
latNextToken = lens _latNextToken (\ s a -> s{_latNextToken = a});

instance AWSRequest ListActionTypes where
        type Sv ListActionTypes = CodePipeline
        type Rs ListActionTypes = ListActionTypesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListActionTypesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "actionTypes" .!@ mempty))

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
              ["actionOwnerFilter" .= _latActionOwnerFilter,
               "nextToken" .= _latNextToken]

instance ToPath ListActionTypes where
        toPath = const "/"

instance ToQuery ListActionTypes where
        toQuery = const mempty

-- | Represents the output of a list action types action.
--
-- /See:/ 'listActionTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'latrsNextToken'
--
-- * 'latrsStatus'
--
-- * 'latrsActionTypes'
data ListActionTypesResponse = ListActionTypesResponse'
    { _latrsNextToken :: !(Maybe Text)
    , _latrsStatus :: !Int
    , _latrsActionTypes :: ![ActionType]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListActionTypesResponse' smart constructor.
listActionTypesResponse :: Int -> ListActionTypesResponse
listActionTypesResponse pStatus_ = 
    ListActionTypesResponse'
    { _latrsNextToken = Nothing
    , _latrsStatus = pStatus_
    , _latrsActionTypes = mempty
    }

-- | If the amount of returned information is significantly large, an
-- identifier is also returned which can be used in a subsequent list
-- action types call to return the next set of action types in the list.
latrsNextToken :: Lens' ListActionTypesResponse (Maybe Text)
latrsNextToken = lens _latrsNextToken (\ s a -> s{_latrsNextToken = a});

-- | Undocumented member.
latrsStatus :: Lens' ListActionTypesResponse Int
latrsStatus = lens _latrsStatus (\ s a -> s{_latrsStatus = a});

-- | Provides details of the action types.
latrsActionTypes :: Lens' ListActionTypesResponse [ActionType]
latrsActionTypes = lens _latrsActionTypes (\ s a -> s{_latrsActionTypes = a}) . _Coerce;
