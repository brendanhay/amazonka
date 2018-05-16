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
-- Module      : Network.AWS.GuardDuty.GetIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the IPSet specified by the IPSet ID.
module Network.AWS.GuardDuty.GetIPSet
    (
    -- * Creating a Request
      getIPSet
    , GetIPSet
    -- * Request Lenses
    , gisDetectorId
    , gisIPSetId

    -- * Destructuring the Response
    , getIPSetResponse
    , GetIPSetResponse
    -- * Response Lenses
    , gisrsStatus
    , gisrsLocation
    , gisrsFormat
    , gisrsName
    , gisrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIPSet' smart constructor.
data GetIPSet = GetIPSet'
  { _gisDetectorId :: !Text
  , _gisIPSetId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisDetectorId' - The detectorID that specifies the GuardDuty service whose IPSet you want to retrieve.
--
-- * 'gisIPSetId' - The unique ID that specifies the IPSet that you want to describe.
getIPSet
    :: Text -- ^ 'gisDetectorId'
    -> Text -- ^ 'gisIPSetId'
    -> GetIPSet
getIPSet pDetectorId_ pIPSetId_ =
  GetIPSet' {_gisDetectorId = pDetectorId_, _gisIPSetId = pIPSetId_}


-- | The detectorID that specifies the GuardDuty service whose IPSet you want to retrieve.
gisDetectorId :: Lens' GetIPSet Text
gisDetectorId = lens _gisDetectorId (\ s a -> s{_gisDetectorId = a})

-- | The unique ID that specifies the IPSet that you want to describe.
gisIPSetId :: Lens' GetIPSet Text
gisIPSetId = lens _gisIPSetId (\ s a -> s{_gisIPSetId = a})

instance AWSRequest GetIPSet where
        type Rs GetIPSet = GetIPSetResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetIPSetResponse' <$>
                   (x .?> "status") <*> (x .?> "location") <*>
                     (x .?> "format")
                     <*> (x .?> "name")
                     <*> (pure (fromEnum s)))

instance Hashable GetIPSet where

instance NFData GetIPSet where

instance ToHeaders GetIPSet where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIPSet where
        toPath GetIPSet'{..}
          = mconcat
              ["/detector/", toBS _gisDetectorId, "/ipset/",
               toBS _gisIPSetId]

instance ToQuery GetIPSet where
        toQuery = const mempty

-- | /See:/ 'getIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { _gisrsStatus         :: !(Maybe IPSetStatus)
  , _gisrsLocation       :: !(Maybe Text)
  , _gisrsFormat         :: !(Maybe IPSetFormat)
  , _gisrsName           :: !(Maybe Text)
  , _gisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisrsStatus' - The status of ipSet file uploaded.
--
-- * 'gisrsLocation' - The URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
--
-- * 'gisrsFormat' - The format of the file that contains the IPSet.
--
-- * 'gisrsName' - The user friendly name to identify the IPSet. This name is displayed in all findings that are triggered by activity that involves IP addresses included in this IPSet.
--
-- * 'gisrsResponseStatus' - -- | The response status code.
getIPSetResponse
    :: Int -- ^ 'gisrsResponseStatus'
    -> GetIPSetResponse
getIPSetResponse pResponseStatus_ =
  GetIPSetResponse'
    { _gisrsStatus = Nothing
    , _gisrsLocation = Nothing
    , _gisrsFormat = Nothing
    , _gisrsName = Nothing
    , _gisrsResponseStatus = pResponseStatus_
    }


-- | The status of ipSet file uploaded.
gisrsStatus :: Lens' GetIPSetResponse (Maybe IPSetStatus)
gisrsStatus = lens _gisrsStatus (\ s a -> s{_gisrsStatus = a})

-- | The URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
gisrsLocation :: Lens' GetIPSetResponse (Maybe Text)
gisrsLocation = lens _gisrsLocation (\ s a -> s{_gisrsLocation = a})

-- | The format of the file that contains the IPSet.
gisrsFormat :: Lens' GetIPSetResponse (Maybe IPSetFormat)
gisrsFormat = lens _gisrsFormat (\ s a -> s{_gisrsFormat = a})

-- | The user friendly name to identify the IPSet. This name is displayed in all findings that are triggered by activity that involves IP addresses included in this IPSet.
gisrsName :: Lens' GetIPSetResponse (Maybe Text)
gisrsName = lens _gisrsName (\ s a -> s{_gisrsName = a})

-- | -- | The response status code.
gisrsResponseStatus :: Lens' GetIPSetResponse Int
gisrsResponseStatus = lens _gisrsResponseStatus (\ s a -> s{_gisrsResponseStatus = a})

instance NFData GetIPSetResponse where
