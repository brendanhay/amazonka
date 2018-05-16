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
-- Module      : Network.AWS.Shield.DeleteProtection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Shield Advanced 'Protection' .
--
--
module Network.AWS.Shield.DeleteProtection
    (
    -- * Creating a Request
      deleteProtection
    , DeleteProtection
    -- * Request Lenses
    , dProtectionId

    -- * Destructuring the Response
    , deleteProtectionResponse
    , DeleteProtectionResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'deleteProtection' smart constructor.
newtype DeleteProtection = DeleteProtection'
  { _dProtectionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dProtectionId' - The unique identifier (ID) for the 'Protection' object to be deleted.
deleteProtection
    :: Text -- ^ 'dProtectionId'
    -> DeleteProtection
deleteProtection pProtectionId_ =
  DeleteProtection' {_dProtectionId = pProtectionId_}


-- | The unique identifier (ID) for the 'Protection' object to be deleted.
dProtectionId :: Lens' DeleteProtection Text
dProtectionId = lens _dProtectionId (\ s a -> s{_dProtectionId = a})

instance AWSRequest DeleteProtection where
        type Rs DeleteProtection = DeleteProtectionResponse
        request = postJSON shield
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProtectionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteProtection where

instance NFData DeleteProtection where

instance ToHeaders DeleteProtection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.DeleteProtection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProtection where
        toJSON DeleteProtection'{..}
          = object
              (catMaybes [Just ("ProtectionId" .= _dProtectionId)])

instance ToPath DeleteProtection where
        toPath = const "/"

instance ToQuery DeleteProtection where
        toQuery = const mempty

-- | /See:/ 'deleteProtectionResponse' smart constructor.
newtype DeleteProtectionResponse = DeleteProtectionResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProtectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteProtectionResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteProtectionResponse
deleteProtectionResponse pResponseStatus_ =
  DeleteProtectionResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteProtectionResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteProtectionResponse where
