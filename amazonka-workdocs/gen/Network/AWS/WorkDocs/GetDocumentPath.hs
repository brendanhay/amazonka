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
-- Module      : Network.AWS.WorkDocs.GetDocumentPath
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the requested document.
--
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.
--
module Network.AWS.WorkDocs.GetDocumentPath
    (
    -- * Creating a Request
      getDocumentPath
    , GetDocumentPath
    -- * Request Lenses
    , gdpAuthenticationToken
    , gdpMarker
    , gdpLimit
    , gdpFields
    , gdpDocumentId

    -- * Destructuring the Response
    , getDocumentPathResponse
    , GetDocumentPathResponse
    -- * Response Lenses
    , gdprsPath
    , gdprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getDocumentPath' smart constructor.
data GetDocumentPath = GetDocumentPath'
  { _gdpAuthenticationToken :: !(Maybe (Sensitive Text))
  , _gdpMarker              :: !(Maybe Text)
  , _gdpLimit               :: !(Maybe Nat)
  , _gdpFields              :: !(Maybe Text)
  , _gdpDocumentId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'gdpMarker' - This value is not supported.
--
-- * 'gdpLimit' - The maximum number of levels in the hierarchy to return.
--
-- * 'gdpFields' - A comma-separated list of values. Specify @NAME@ to include the names of the parent folders.
--
-- * 'gdpDocumentId' - The ID of the document.
getDocumentPath
    :: Text -- ^ 'gdpDocumentId'
    -> GetDocumentPath
getDocumentPath pDocumentId_ =
  GetDocumentPath'
    { _gdpAuthenticationToken = Nothing
    , _gdpMarker = Nothing
    , _gdpLimit = Nothing
    , _gdpFields = Nothing
    , _gdpDocumentId = pDocumentId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
gdpAuthenticationToken :: Lens' GetDocumentPath (Maybe Text)
gdpAuthenticationToken = lens _gdpAuthenticationToken (\ s a -> s{_gdpAuthenticationToken = a}) . mapping _Sensitive

-- | This value is not supported.
gdpMarker :: Lens' GetDocumentPath (Maybe Text)
gdpMarker = lens _gdpMarker (\ s a -> s{_gdpMarker = a})

-- | The maximum number of levels in the hierarchy to return.
gdpLimit :: Lens' GetDocumentPath (Maybe Natural)
gdpLimit = lens _gdpLimit (\ s a -> s{_gdpLimit = a}) . mapping _Nat

-- | A comma-separated list of values. Specify @NAME@ to include the names of the parent folders.
gdpFields :: Lens' GetDocumentPath (Maybe Text)
gdpFields = lens _gdpFields (\ s a -> s{_gdpFields = a})

-- | The ID of the document.
gdpDocumentId :: Lens' GetDocumentPath Text
gdpDocumentId = lens _gdpDocumentId (\ s a -> s{_gdpDocumentId = a})

instance AWSRequest GetDocumentPath where
        type Rs GetDocumentPath = GetDocumentPathResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetDocumentPathResponse' <$>
                   (x .?> "Path") <*> (pure (fromEnum s)))

instance Hashable GetDocumentPath where

instance NFData GetDocumentPath where

instance ToHeaders GetDocumentPath where
        toHeaders GetDocumentPath'{..}
          = mconcat
              ["Authentication" =# _gdpAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath GetDocumentPath where
        toPath GetDocumentPath'{..}
          = mconcat
              ["/api/v1/documents/", toBS _gdpDocumentId, "/path"]

instance ToQuery GetDocumentPath where
        toQuery GetDocumentPath'{..}
          = mconcat
              ["marker" =: _gdpMarker, "limit" =: _gdpLimit,
               "fields" =: _gdpFields]

-- | /See:/ 'getDocumentPathResponse' smart constructor.
data GetDocumentPathResponse = GetDocumentPathResponse'
  { _gdprsPath           :: !(Maybe ResourcePath)
  , _gdprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentPathResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdprsPath' - The path information.
--
-- * 'gdprsResponseStatus' - -- | The response status code.
getDocumentPathResponse
    :: Int -- ^ 'gdprsResponseStatus'
    -> GetDocumentPathResponse
getDocumentPathResponse pResponseStatus_ =
  GetDocumentPathResponse'
    {_gdprsPath = Nothing, _gdprsResponseStatus = pResponseStatus_}


-- | The path information.
gdprsPath :: Lens' GetDocumentPathResponse (Maybe ResourcePath)
gdprsPath = lens _gdprsPath (\ s a -> s{_gdprsPath = a})

-- | -- | The response status code.
gdprsResponseStatus :: Lens' GetDocumentPathResponse Int
gdprsResponseStatus = lens _gdprsResponseStatus (\ s a -> s{_gdprsResponseStatus = a})

instance NFData GetDocumentPathResponse where
