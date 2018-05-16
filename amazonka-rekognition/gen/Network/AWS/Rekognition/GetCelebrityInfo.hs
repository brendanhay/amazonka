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
-- Module      : Network.AWS.Rekognition.GetCelebrityInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the name and additional information about a celebrity based on his or her Rekognition ID. The additional information is returned as an array of URLs. If there is no additional information about the celebrity, this list is empty. For more information, see 'get-celebrity-info-procedure' .
--
--
-- This operation requires permissions to perform the @rekognition:GetCelebrityInfo@ action.
--
module Network.AWS.Rekognition.GetCelebrityInfo
    (
    -- * Creating a Request
      getCelebrityInfo
    , GetCelebrityInfo
    -- * Request Lenses
    , gciId

    -- * Destructuring the Response
    , getCelebrityInfoResponse
    , GetCelebrityInfoResponse
    -- * Response Lenses
    , gcirsURLs
    , gcirsName
    , gcirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCelebrityInfo' smart constructor.
newtype GetCelebrityInfo = GetCelebrityInfo'
  { _gciId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCelebrityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gciId' - The ID for the celebrity. You get the celebrity ID from a call to the operation, which recognizes celebrities in an image.
getCelebrityInfo
    :: Text -- ^ 'gciId'
    -> GetCelebrityInfo
getCelebrityInfo pId_ = GetCelebrityInfo' {_gciId = pId_}


-- | The ID for the celebrity. You get the celebrity ID from a call to the operation, which recognizes celebrities in an image.
gciId :: Lens' GetCelebrityInfo Text
gciId = lens _gciId (\ s a -> s{_gciId = a})

instance AWSRequest GetCelebrityInfo where
        type Rs GetCelebrityInfo = GetCelebrityInfoResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 GetCelebrityInfoResponse' <$>
                   (x .?> "Urls" .!@ mempty) <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance Hashable GetCelebrityInfo where

instance NFData GetCelebrityInfo where

instance ToHeaders GetCelebrityInfo where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.GetCelebrityInfo" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCelebrityInfo where
        toJSON GetCelebrityInfo'{..}
          = object (catMaybes [Just ("Id" .= _gciId)])

instance ToPath GetCelebrityInfo where
        toPath = const "/"

instance ToQuery GetCelebrityInfo where
        toQuery = const mempty

-- | /See:/ 'getCelebrityInfoResponse' smart constructor.
data GetCelebrityInfoResponse = GetCelebrityInfoResponse'
  { _gcirsURLs           :: !(Maybe [Text])
  , _gcirsName           :: !(Maybe Text)
  , _gcirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCelebrityInfoResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirsURLs' - An array of URLs pointing to additional celebrity information.
--
-- * 'gcirsName' - The name of the celebrity.
--
-- * 'gcirsResponseStatus' - -- | The response status code.
getCelebrityInfoResponse
    :: Int -- ^ 'gcirsResponseStatus'
    -> GetCelebrityInfoResponse
getCelebrityInfoResponse pResponseStatus_ =
  GetCelebrityInfoResponse'
    { _gcirsURLs = Nothing
    , _gcirsName = Nothing
    , _gcirsResponseStatus = pResponseStatus_
    }


-- | An array of URLs pointing to additional celebrity information.
gcirsURLs :: Lens' GetCelebrityInfoResponse [Text]
gcirsURLs = lens _gcirsURLs (\ s a -> s{_gcirsURLs = a}) . _Default . _Coerce

-- | The name of the celebrity.
gcirsName :: Lens' GetCelebrityInfoResponse (Maybe Text)
gcirsName = lens _gcirsName (\ s a -> s{_gcirsName = a})

-- | -- | The response status code.
gcirsResponseStatus :: Lens' GetCelebrityInfoResponse Int
gcirsResponseStatus = lens _gcirsResponseStatus (\ s a -> s{_gcirsResponseStatus = a})

instance NFData GetCelebrityInfoResponse where
