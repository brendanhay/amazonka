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
-- Module      : Network.AWS.Lightsail.DeleteKeyPair
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific SSH key pair.
--
--
module Network.AWS.Lightsail.DeleteKeyPair
    (
    -- * Creating a Request
      deleteKeyPair
    , DeleteKeyPair
    -- * Request Lenses
    , dkpKeyPairName

    -- * Destructuring the Response
    , deleteKeyPairResponse
    , DeleteKeyPairResponse
    -- * Response Lenses
    , dkprsOperation
    , dkprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteKeyPair' smart constructor.
newtype DeleteKeyPair = DeleteKeyPair'
  { _dkpKeyPairName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteKeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkpKeyPairName' - The name of the key pair to delete.
deleteKeyPair
    :: Text -- ^ 'dkpKeyPairName'
    -> DeleteKeyPair
deleteKeyPair pKeyPairName_ = DeleteKeyPair' {_dkpKeyPairName = pKeyPairName_}


-- | The name of the key pair to delete.
dkpKeyPairName :: Lens' DeleteKeyPair Text
dkpKeyPairName = lens _dkpKeyPairName (\ s a -> s{_dkpKeyPairName = a})

instance AWSRequest DeleteKeyPair where
        type Rs DeleteKeyPair = DeleteKeyPairResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteKeyPairResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable DeleteKeyPair where

instance NFData DeleteKeyPair where

instance ToHeaders DeleteKeyPair where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteKeyPair" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteKeyPair where
        toJSON DeleteKeyPair'{..}
          = object
              (catMaybes [Just ("keyPairName" .= _dkpKeyPairName)])

instance ToPath DeleteKeyPair where
        toPath = const "/"

instance ToQuery DeleteKeyPair where
        toQuery = const mempty

-- | /See:/ 'deleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  { _dkprsOperation      :: !(Maybe Operation)
  , _dkprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteKeyPairResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkprsOperation' - An array of key-value pairs containing information about the results of your delete key pair request.
--
-- * 'dkprsResponseStatus' - -- | The response status code.
deleteKeyPairResponse
    :: Int -- ^ 'dkprsResponseStatus'
    -> DeleteKeyPairResponse
deleteKeyPairResponse pResponseStatus_ =
  DeleteKeyPairResponse'
    {_dkprsOperation = Nothing, _dkprsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your delete key pair request.
dkprsOperation :: Lens' DeleteKeyPairResponse (Maybe Operation)
dkprsOperation = lens _dkprsOperation (\ s a -> s{_dkprsOperation = a})

-- | -- | The response status code.
dkprsResponseStatus :: Lens' DeleteKeyPairResponse Int
dkprsResponseStatus = lens _dkprsResponseStatus (\ s a -> s{_dkprsResponseStatus = a})

instance NFData DeleteKeyPairResponse where
