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
    , larqNextToken
    , larqArn
    , larqType

    -- * Response
    , ListArtifactsResponse
    -- ** Response constructor
    , listArtifactsResponse
    -- ** Response lenses
    , larsArtifacts
    , larsNextToken
    , larsStatus
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
-- * 'larqNextToken'
--
-- * 'larqArn'
--
-- * 'larqType'
data ListArtifacts = ListArtifacts'
    { _larqNextToken :: !(Maybe Text)
    , _larqArn       :: !Text
    , _larqType      :: !ArtifactCategory
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListArtifacts' smart constructor.
listArtifacts :: Text -> ArtifactCategory -> ListArtifacts
listArtifacts pArn_ pType_ =
    ListArtifacts'
    { _larqNextToken = Nothing
    , _larqArn = pArn_
    , _larqType = pType_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
larqNextToken :: Lens' ListArtifacts (Maybe Text)
larqNextToken = lens _larqNextToken (\ s a -> s{_larqNextToken = a});

-- | The artifacts\' ARNs.
larqArn :: Lens' ListArtifacts Text
larqArn = lens _larqArn (\ s a -> s{_larqArn = a});

-- | The artifacts\' type.
--
-- Allowed values include:
--
-- -   FILE: The artifacts are files.
-- -   LOG: The artifacts are logs.
-- -   SCREENSHOT: The artifacts are screenshots.
larqType :: Lens' ListArtifacts ArtifactCategory
larqType = lens _larqType (\ s a -> s{_larqType = a});

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
              ["nextToken" .= _larqNextToken, "arn" .= _larqArn,
               "type" .= _larqType]

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
-- * 'larsArtifacts'
--
-- * 'larsNextToken'
--
-- * 'larsStatus'
data ListArtifactsResponse = ListArtifactsResponse'
    { _larsArtifacts :: !(Maybe [Artifact])
    , _larsNextToken :: !(Maybe Text)
    , _larsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListArtifactsResponse' smart constructor.
listArtifactsResponse :: Int -> ListArtifactsResponse
listArtifactsResponse pStatus_ =
    ListArtifactsResponse'
    { _larsArtifacts = Nothing
    , _larsNextToken = Nothing
    , _larsStatus = pStatus_
    }

-- | Information about the artifacts.
larsArtifacts :: Lens' ListArtifactsResponse [Artifact]
larsArtifacts = lens _larsArtifacts (\ s a -> s{_larsArtifacts = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
larsNextToken :: Lens' ListArtifactsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a});

-- | FIXME: Undocumented member.
larsStatus :: Lens' ListArtifactsResponse Int
larsStatus = lens _larsStatus (\ s a -> s{_larsStatus = a});
