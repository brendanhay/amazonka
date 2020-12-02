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
-- Module      : Network.AWS.Pinpoint.GetImportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an import job.
module Network.AWS.Pinpoint.GetImportJob
    (
    -- * Creating a Request
      getImportJob
    , GetImportJob
    -- * Request Lenses
    , gijApplicationId
    , gijJobId

    -- * Destructuring the Response
    , getImportJobResponse
    , GetImportJobResponse
    -- * Response Lenses
    , gijrsResponseStatus
    , gijrsImportJobResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getImportJob' smart constructor.
data GetImportJob = GetImportJob'
  { _gijApplicationId :: !Text
  , _gijJobId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetImportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gijApplicationId' - Undocumented member.
--
-- * 'gijJobId' - Undocumented member.
getImportJob
    :: Text -- ^ 'gijApplicationId'
    -> Text -- ^ 'gijJobId'
    -> GetImportJob
getImportJob pApplicationId_ pJobId_ =
  GetImportJob' {_gijApplicationId = pApplicationId_, _gijJobId = pJobId_}


-- | Undocumented member.
gijApplicationId :: Lens' GetImportJob Text
gijApplicationId = lens _gijApplicationId (\ s a -> s{_gijApplicationId = a})

-- | Undocumented member.
gijJobId :: Lens' GetImportJob Text
gijJobId = lens _gijJobId (\ s a -> s{_gijJobId = a})

instance AWSRequest GetImportJob where
        type Rs GetImportJob = GetImportJobResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetImportJobResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetImportJob where

instance NFData GetImportJob where

instance ToHeaders GetImportJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetImportJob where
        toPath GetImportJob'{..}
          = mconcat
              ["/v1/apps/", toBS _gijApplicationId,
               "/jobs/import/", toBS _gijJobId]

instance ToQuery GetImportJob where
        toQuery = const mempty

-- | /See:/ 'getImportJobResponse' smart constructor.
data GetImportJobResponse = GetImportJobResponse'
  { _gijrsResponseStatus    :: !Int
  , _gijrsImportJobResponse :: !ImportJobResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gijrsResponseStatus' - -- | The response status code.
--
-- * 'gijrsImportJobResponse' - Undocumented member.
getImportJobResponse
    :: Int -- ^ 'gijrsResponseStatus'
    -> ImportJobResponse -- ^ 'gijrsImportJobResponse'
    -> GetImportJobResponse
getImportJobResponse pResponseStatus_ pImportJobResponse_ =
  GetImportJobResponse'
    { _gijrsResponseStatus = pResponseStatus_
    , _gijrsImportJobResponse = pImportJobResponse_
    }


-- | -- | The response status code.
gijrsResponseStatus :: Lens' GetImportJobResponse Int
gijrsResponseStatus = lens _gijrsResponseStatus (\ s a -> s{_gijrsResponseStatus = a})

-- | Undocumented member.
gijrsImportJobResponse :: Lens' GetImportJobResponse ImportJobResponse
gijrsImportJobResponse = lens _gijrsImportJobResponse (\ s a -> s{_gijrsImportJobResponse = a})

instance NFData GetImportJobResponse where
