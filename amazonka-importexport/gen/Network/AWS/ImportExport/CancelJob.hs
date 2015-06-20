{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ImportExport.CancelJob
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation cancels a specified job. Only the job owner can cancel
-- it. The operation fails if the job has already started or is complete.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCancelJob.html>
module Network.AWS.ImportExport.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , canAPIVersion
    , canJobId

    -- * Response
    , CancelJobResponse
    -- ** Response constructor
    , cancelJobResponse
    -- ** Response lenses
    , cjrSuccess
    ) where

import Network.AWS.ImportExport.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canAPIVersion'
--
-- * 'canJobId'
data CancelJob = CancelJob'{_canAPIVersion :: Maybe Text, _canJobId :: Text} deriving (Eq, Read, Show)

-- | 'CancelJob' smart constructor.
cancelJob :: Text -> CancelJob
cancelJob pJobId = CancelJob'{_canAPIVersion = Nothing, _canJobId = pJobId};

-- | FIXME: Undocumented member.
canAPIVersion :: Lens' CancelJob (Maybe Text)
canAPIVersion = lens _canAPIVersion (\ s a -> s{_canAPIVersion = a});

-- | FIXME: Undocumented member.
canJobId :: Lens' CancelJob Text
canJobId = lens _canJobId (\ s a -> s{_canJobId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest CancelJob where
        type Sv CancelJob = ImportExport
        type Rs CancelJob = CancelJobResponse
        request = post
        response
          = receiveXMLWrapper "CancelJobResult"
              (\ s h x -> CancelJobResponse' <$> (x .@? "Success"))

instance ToHeaders CancelJob where
        toHeaders = const mempty

instance ToPath CancelJob where
        toPath = const "/"

instance ToQuery CancelJob where
        toQuery CancelJob'{..}
          = mconcat
              ["Operation=CancelJob",
               "Action" =: ("CancelJob" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "APIVersion" =: _canAPIVersion, "JobId" =: _canJobId]

-- | /See:/ 'cancelJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrSuccess'
newtype CancelJobResponse = CancelJobResponse'{_cjrSuccess :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'CancelJobResponse' smart constructor.
cancelJobResponse :: CancelJobResponse
cancelJobResponse = CancelJobResponse'{_cjrSuccess = Nothing};

-- | FIXME: Undocumented member.
cjrSuccess :: Lens' CancelJobResponse (Maybe Bool)
cjrSuccess = lens _cjrSuccess (\ s a -> s{_cjrSuccess = a});
