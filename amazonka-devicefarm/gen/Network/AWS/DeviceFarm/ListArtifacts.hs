{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListArtifacts
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about artifacts.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListArtifacts.html>
module Network.AWS.DeviceFarm.ListArtifacts
    (
    -- * Request
      ListArtifacts
    -- ** Request constructor
    , listArtifacts
    -- ** Request lenses
    , laNextToken
    , laArn
    , laType

    -- * Response
    , ListArtifactsResponse
    -- ** Response constructor
    , listArtifactsResponse
    -- ** Response lenses
    , larArtifacts
    , larNextToken
    , larStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list artifacts operation.
--
-- /See:/ 'listArtifacts' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laNextToken'
--
-- * 'laArn'
--
-- * 'laType'
data ListArtifacts = ListArtifacts'
    { _laNextToken :: !(Maybe Text)
    , _laArn       :: !Text
    , _laType      :: !ArtifactCategory
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListArtifacts' smart constructor.
listArtifacts :: Text -> ArtifactCategory -> ListArtifacts
listArtifacts pArn pType =
    ListArtifacts'
    { _laNextToken = Nothing
    , _laArn = pArn
    , _laType = pType
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
laNextToken :: Lens' ListArtifacts (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a});

-- | The artifacts\' ARNs.
laArn :: Lens' ListArtifacts Text
laArn = lens _laArn (\ s a -> s{_laArn = a});

-- | The artifacts\' type.
--
-- Allowed values include:
--
-- -   FILE: The artifacts are files.
-- -   LOG: The artifacts are logs.
-- -   SCREENSHOT: The artifacts are screenshots.
laType :: Lens' ListArtifacts ArtifactCategory
laType = lens _laType (\ s a -> s{_laType = a});

instance AWSRequest ListArtifacts where
        type Sv ListArtifacts = DeviceFarm
        type Rs ListArtifacts = ListArtifactsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListArtifactsResponse' <$>
                   (x .?> "artifacts" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

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
              ["nextToken" .= _laNextToken, "arn" .= _laArn,
               "type" .= _laType]

instance ToPath ListArtifacts where
        toPath = const "/"

instance ToQuery ListArtifacts where
        toQuery = const mempty

-- | Represents the result of a list artifacts operation.
--
-- /See:/ 'listArtifactsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larArtifacts'
--
-- * 'larNextToken'
--
-- * 'larStatus'
data ListArtifactsResponse = ListArtifactsResponse'
    { _larArtifacts :: !(Maybe [Artifact])
    , _larNextToken :: !(Maybe Text)
    , _larStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListArtifactsResponse' smart constructor.
listArtifactsResponse :: Int -> ListArtifactsResponse
listArtifactsResponse pStatus =
    ListArtifactsResponse'
    { _larArtifacts = Nothing
    , _larNextToken = Nothing
    , _larStatus = pStatus
    }

-- | Information about the artifacts.
larArtifacts :: Lens' ListArtifactsResponse [Artifact]
larArtifacts = lens _larArtifacts (\ s a -> s{_larArtifacts = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
larNextToken :: Lens' ListArtifactsResponse (Maybe Text)
larNextToken = lens _larNextToken (\ s a -> s{_larNextToken = a});

-- | FIXME: Undocumented member.
larStatus :: Lens' ListArtifactsResponse Int
larStatus = lens _larStatus (\ s a -> s{_larStatus = a});
