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
-- Module      : Network.AWS.CloudDirectory.ListAppliedSchemaARNs
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists schemas applied to a directory.
--
--
module Network.AWS.CloudDirectory.ListAppliedSchemaARNs
    (
    -- * Creating a Request
      listAppliedSchemaARNs
    , ListAppliedSchemaARNs
    -- * Request Lenses
    , lasaNextToken
    , lasaMaxResults
    , lasaDirectoryARN

    -- * Destructuring the Response
    , listAppliedSchemaARNsResponse
    , ListAppliedSchemaARNsResponse
    -- * Response Lenses
    , lasarsSchemaARNs
    , lasarsNextToken
    , lasarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAppliedSchemaARNs' smart constructor.
data ListAppliedSchemaARNs = ListAppliedSchemaARNs'
  { _lasaNextToken    :: !(Maybe Text)
  , _lasaMaxResults   :: !(Maybe Nat)
  , _lasaDirectoryARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAppliedSchemaARNs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasaNextToken' - The pagination token.
--
-- * 'lasaMaxResults' - The maximum number of results to retrieve.
--
-- * 'lasaDirectoryARN' - The ARN of the directory you are listing.
listAppliedSchemaARNs
    :: Text -- ^ 'lasaDirectoryARN'
    -> ListAppliedSchemaARNs
listAppliedSchemaARNs pDirectoryARN_ =
  ListAppliedSchemaARNs'
  { _lasaNextToken = Nothing
  , _lasaMaxResults = Nothing
  , _lasaDirectoryARN = pDirectoryARN_
  }


-- | The pagination token.
lasaNextToken :: Lens' ListAppliedSchemaARNs (Maybe Text)
lasaNextToken = lens _lasaNextToken (\ s a -> s{_lasaNextToken = a});

-- | The maximum number of results to retrieve.
lasaMaxResults :: Lens' ListAppliedSchemaARNs (Maybe Natural)
lasaMaxResults = lens _lasaMaxResults (\ s a -> s{_lasaMaxResults = a}) . mapping _Nat;

-- | The ARN of the directory you are listing.
lasaDirectoryARN :: Lens' ListAppliedSchemaARNs Text
lasaDirectoryARN = lens _lasaDirectoryARN (\ s a -> s{_lasaDirectoryARN = a});

instance AWSRequest ListAppliedSchemaARNs where
        type Rs ListAppliedSchemaARNs =
             ListAppliedSchemaARNsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListAppliedSchemaARNsResponse' <$>
                   (x .?> "SchemaArns" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAppliedSchemaARNs where

instance NFData ListAppliedSchemaARNs where

instance ToHeaders ListAppliedSchemaARNs where
        toHeaders = const mempty

instance ToJSON ListAppliedSchemaARNs where
        toJSON ListAppliedSchemaARNs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lasaNextToken,
                  ("MaxResults" .=) <$> _lasaMaxResults,
                  Just ("DirectoryArn" .= _lasaDirectoryARN)])

instance ToPath ListAppliedSchemaARNs where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/applied"

instance ToQuery ListAppliedSchemaARNs where
        toQuery = const mempty

-- | /See:/ 'listAppliedSchemaARNsResponse' smart constructor.
data ListAppliedSchemaARNsResponse = ListAppliedSchemaARNsResponse'
  { _lasarsSchemaARNs     :: !(Maybe [Text])
  , _lasarsNextToken      :: !(Maybe Text)
  , _lasarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAppliedSchemaARNsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasarsSchemaARNs' - The ARNs of schemas that are applied to the directory.
--
-- * 'lasarsNextToken' - The pagination token.
--
-- * 'lasarsResponseStatus' - -- | The response status code.
listAppliedSchemaARNsResponse
    :: Int -- ^ 'lasarsResponseStatus'
    -> ListAppliedSchemaARNsResponse
listAppliedSchemaARNsResponse pResponseStatus_ =
  ListAppliedSchemaARNsResponse'
  { _lasarsSchemaARNs = Nothing
  , _lasarsNextToken = Nothing
  , _lasarsResponseStatus = pResponseStatus_
  }


-- | The ARNs of schemas that are applied to the directory.
lasarsSchemaARNs :: Lens' ListAppliedSchemaARNsResponse [Text]
lasarsSchemaARNs = lens _lasarsSchemaARNs (\ s a -> s{_lasarsSchemaARNs = a}) . _Default . _Coerce;

-- | The pagination token.
lasarsNextToken :: Lens' ListAppliedSchemaARNsResponse (Maybe Text)
lasarsNextToken = lens _lasarsNextToken (\ s a -> s{_lasarsNextToken = a});

-- | -- | The response status code.
lasarsResponseStatus :: Lens' ListAppliedSchemaARNsResponse Int
lasarsResponseStatus = lens _lasarsResponseStatus (\ s a -> s{_lasarsResponseStatus = a});

instance NFData ListAppliedSchemaARNsResponse where
