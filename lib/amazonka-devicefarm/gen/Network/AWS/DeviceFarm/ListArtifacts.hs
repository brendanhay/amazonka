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
-- Module      : Network.AWS.DeviceFarm.ListArtifacts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about artifacts.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListArtifacts
    (
    -- * Creating a Request
      listArtifacts
    , ListArtifacts
    -- * Request Lenses
    , laNextToken
    , laArn
    , laType

    -- * Destructuring the Response
    , listArtifactsResponse
    , ListArtifactsResponse
    -- * Response Lenses
    , larsArtifacts
    , larsNextToken
    , larsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list artifacts operation.
--
--
--
-- /See:/ 'listArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { _laNextToken :: !(Maybe Text)
  , _laArn       :: !Text
  , _laType      :: !ArtifactCategory
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'laArn' - The Run, Job, Suite, or Test ARN.
--
-- * 'laType' - The artifacts' type. Allowed values include:     * FILE: The artifacts are files.     * LOG: The artifacts are logs.     * SCREENSHOT: The artifacts are screenshots.
listArtifacts
    :: Text -- ^ 'laArn'
    -> ArtifactCategory -- ^ 'laType'
    -> ListArtifacts
listArtifacts pArn_ pType_ =
  ListArtifacts' {_laNextToken = Nothing, _laArn = pArn_, _laType = pType_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
laNextToken :: Lens' ListArtifacts (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | The Run, Job, Suite, or Test ARN.
laArn :: Lens' ListArtifacts Text
laArn = lens _laArn (\ s a -> s{_laArn = a})

-- | The artifacts' type. Allowed values include:     * FILE: The artifacts are files.     * LOG: The artifacts are logs.     * SCREENSHOT: The artifacts are screenshots.
laType :: Lens' ListArtifacts ArtifactCategory
laType = lens _laType (\ s a -> s{_laType = a})

instance AWSPager ListArtifacts where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsArtifacts) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListArtifacts where
        type Rs ListArtifacts = ListArtifactsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListArtifactsResponse' <$>
                   (x .?> "artifacts" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListArtifacts where

instance NFData ListArtifacts where

instance ToHeaders ListArtifacts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListArtifacts" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListArtifacts where
        toJSON ListArtifacts'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _laNextToken,
                  Just ("arn" .= _laArn), Just ("type" .= _laType)])

instance ToPath ListArtifacts where
        toPath = const "/"

instance ToQuery ListArtifacts where
        toQuery = const mempty

-- | Represents the result of a list artifacts operation.
--
--
--
-- /See:/ 'listArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { _larsArtifacts      :: !(Maybe [Artifact])
  , _larsNextToken      :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListArtifactsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsArtifacts' - Information about the artifacts.
--
-- * 'larsNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'larsResponseStatus' - -- | The response status code.
listArtifactsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListArtifactsResponse
listArtifactsResponse pResponseStatus_ =
  ListArtifactsResponse'
    { _larsArtifacts = Nothing
    , _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | Information about the artifacts.
larsArtifacts :: Lens' ListArtifactsResponse [Artifact]
larsArtifacts = lens _larsArtifacts (\ s a -> s{_larsArtifacts = a}) . _Default . _Coerce

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
larsNextToken :: Lens' ListArtifactsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListArtifactsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListArtifactsResponse where
