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
-- Module      : Network.AWS.MechanicalTurk.GetHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetHIT@ operation retrieves the details of the specified HIT.
--
--
module Network.AWS.MechanicalTurk.GetHIT
    (
    -- * Creating a Request
      getHIT
    , GetHIT
    -- * Request Lenses
    , ghitHITId

    -- * Destructuring the Response
    , getHITResponse
    , GetHITResponse
    -- * Response Lenses
    , ghitrsHIT
    , ghitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getHIT' smart constructor.
newtype GetHIT = GetHIT'
  { _ghitHITId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghitHITId' - The ID of the HIT to be retrieved.
getHIT
    :: Text -- ^ 'ghitHITId'
    -> GetHIT
getHIT pHITId_ = GetHIT' {_ghitHITId = pHITId_}


-- | The ID of the HIT to be retrieved.
ghitHITId :: Lens' GetHIT Text
ghitHITId = lens _ghitHITId (\ s a -> s{_ghitHITId = a})

instance AWSRequest GetHIT where
        type Rs GetHIT = GetHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 GetHITResponse' <$>
                   (x .?> "HIT") <*> (pure (fromEnum s)))

instance Hashable GetHIT where

instance NFData GetHIT where

instance ToHeaders GetHIT where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.GetHIT" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetHIT where
        toJSON GetHIT'{..}
          = object (catMaybes [Just ("HITId" .= _ghitHITId)])

instance ToPath GetHIT where
        toPath = const "/"

instance ToQuery GetHIT where
        toQuery = const mempty

-- | /See:/ 'getHITResponse' smart constructor.
data GetHITResponse = GetHITResponse'
  { _ghitrsHIT            :: !(Maybe HIT)
  , _ghitrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghitrsHIT' - Contains the requested HIT data.
--
-- * 'ghitrsResponseStatus' - -- | The response status code.
getHITResponse
    :: Int -- ^ 'ghitrsResponseStatus'
    -> GetHITResponse
getHITResponse pResponseStatus_ =
  GetHITResponse'
    {_ghitrsHIT = Nothing, _ghitrsResponseStatus = pResponseStatus_}


-- | Contains the requested HIT data.
ghitrsHIT :: Lens' GetHITResponse (Maybe HIT)
ghitrsHIT = lens _ghitrsHIT (\ s a -> s{_ghitrsHIT = a})

-- | -- | The response status code.
ghitrsResponseStatus :: Lens' GetHITResponse Int
ghitrsResponseStatus = lens _ghitrsResponseStatus (\ s a -> s{_ghitrsResponseStatus = a})

instance NFData GetHITResponse where
