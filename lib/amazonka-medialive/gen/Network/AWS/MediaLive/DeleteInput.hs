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
-- Module      : Network.AWS.MediaLive.DeleteInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the input end point
module Network.AWS.MediaLive.DeleteInput
    (
    -- * Creating a Request
      deleteInput
    , DeleteInput
    -- * Request Lenses
    , diInputId

    -- * Destructuring the Response
    , deleteInputResponse
    , DeleteInputResponse
    -- * Response Lenses
    , dirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteInputRequest
--
-- /See:/ 'deleteInput' smart constructor.
newtype DeleteInput = DeleteInput'
  { _diInputId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diInputId' - Unique ID of the input
deleteInput
    :: Text -- ^ 'diInputId'
    -> DeleteInput
deleteInput pInputId_ = DeleteInput' {_diInputId = pInputId_}


-- | Unique ID of the input
diInputId :: Lens' DeleteInput Text
diInputId = lens _diInputId (\ s a -> s{_diInputId = a})

instance AWSRequest DeleteInput where
        type Rs DeleteInput = DeleteInputResponse
        request = delete mediaLive
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteInputResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteInput where

instance NFData DeleteInput where

instance ToHeaders DeleteInput where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteInput where
        toPath DeleteInput'{..}
          = mconcat ["/prod/inputs/", toBS _diInputId]

instance ToQuery DeleteInput where
        toQuery = const mempty

-- | Placeholder documentation for DeleteInputResponse
--
-- /See:/ 'deleteInputResponse' smart constructor.
newtype DeleteInputResponse = DeleteInputResponse'
  { _dirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteInputResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeleteInputResponse
deleteInputResponse pResponseStatus_ =
  DeleteInputResponse' {_dirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteInputResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DeleteInputResponse where
