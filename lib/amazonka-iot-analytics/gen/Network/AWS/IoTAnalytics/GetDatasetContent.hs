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
-- Module      : Network.AWS.IoTAnalytics.GetDatasetContent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of a data set as pre-signed URIs.
--
--
module Network.AWS.IoTAnalytics.GetDatasetContent
    (
    -- * Creating a Request
      getDatasetContent
    , GetDatasetContent
    -- * Request Lenses
    , gdcVersionId
    , gdcDatasetName

    -- * Destructuring the Response
    , getDatasetContentResponse
    , GetDatasetContentResponse
    -- * Response Lenses
    , gdcrsStatus
    , gdcrsEntries
    , gdcrsTimestamp
    , gdcrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDatasetContent' smart constructor.
data GetDatasetContent = GetDatasetContent'
  { _gdcVersionId   :: !(Maybe Text)
  , _gdcDatasetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDatasetContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcVersionId' - The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- * 'gdcDatasetName' - The name of the data set whose contents are retrieved.
getDatasetContent
    :: Text -- ^ 'gdcDatasetName'
    -> GetDatasetContent
getDatasetContent pDatasetName_ =
  GetDatasetContent' {_gdcVersionId = Nothing, _gdcDatasetName = pDatasetName_}


-- | The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
gdcVersionId :: Lens' GetDatasetContent (Maybe Text)
gdcVersionId = lens _gdcVersionId (\ s a -> s{_gdcVersionId = a})

-- | The name of the data set whose contents are retrieved.
gdcDatasetName :: Lens' GetDatasetContent Text
gdcDatasetName = lens _gdcDatasetName (\ s a -> s{_gdcDatasetName = a})

instance AWSRequest GetDatasetContent where
        type Rs GetDatasetContent = GetDatasetContentResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 GetDatasetContentResponse' <$>
                   (x .?> "status") <*> (x .?> "entries" .!@ mempty) <*>
                     (x .?> "timestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetDatasetContent where

instance NFData GetDatasetContent where

instance ToHeaders GetDatasetContent where
        toHeaders = const mempty

instance ToPath GetDatasetContent where
        toPath GetDatasetContent'{..}
          = mconcat
              ["/datasets/", toBS _gdcDatasetName, "/content"]

instance ToQuery GetDatasetContent where
        toQuery GetDatasetContent'{..}
          = mconcat ["versionId" =: _gdcVersionId]

-- | /See:/ 'getDatasetContentResponse' smart constructor.
data GetDatasetContentResponse = GetDatasetContentResponse'
  { _gdcrsStatus         :: !(Maybe DatasetContentStatus)
  , _gdcrsEntries        :: !(Maybe [DatasetEntry])
  , _gdcrsTimestamp      :: !(Maybe POSIX)
  , _gdcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDatasetContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcrsStatus' - The status of the data set content.
--
-- * 'gdcrsEntries' - A list of "DatasetEntry" objects.
--
-- * 'gdcrsTimestamp' - The time when the request was made.
--
-- * 'gdcrsResponseStatus' - -- | The response status code.
getDatasetContentResponse
    :: Int -- ^ 'gdcrsResponseStatus'
    -> GetDatasetContentResponse
getDatasetContentResponse pResponseStatus_ =
  GetDatasetContentResponse'
    { _gdcrsStatus = Nothing
    , _gdcrsEntries = Nothing
    , _gdcrsTimestamp = Nothing
    , _gdcrsResponseStatus = pResponseStatus_
    }


-- | The status of the data set content.
gdcrsStatus :: Lens' GetDatasetContentResponse (Maybe DatasetContentStatus)
gdcrsStatus = lens _gdcrsStatus (\ s a -> s{_gdcrsStatus = a})

-- | A list of "DatasetEntry" objects.
gdcrsEntries :: Lens' GetDatasetContentResponse [DatasetEntry]
gdcrsEntries = lens _gdcrsEntries (\ s a -> s{_gdcrsEntries = a}) . _Default . _Coerce

-- | The time when the request was made.
gdcrsTimestamp :: Lens' GetDatasetContentResponse (Maybe UTCTime)
gdcrsTimestamp = lens _gdcrsTimestamp (\ s a -> s{_gdcrsTimestamp = a}) . mapping _Time

-- | -- | The response status code.
gdcrsResponseStatus :: Lens' GetDatasetContentResponse Int
gdcrsResponseStatus = lens _gdcrsResponseStatus (\ s a -> s{_gdcrsResponseStatus = a})

instance NFData GetDatasetContentResponse where
