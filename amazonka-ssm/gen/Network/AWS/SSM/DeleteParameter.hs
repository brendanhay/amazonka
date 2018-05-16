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
-- Module      : Network.AWS.SSM.DeleteParameter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a parameter from the system.
--
--
module Network.AWS.SSM.DeleteParameter
    (
    -- * Creating a Request
      deleteParameter
    , DeleteParameter
    -- * Request Lenses
    , delName

    -- * Destructuring the Response
    , deleteParameterResponse
    , DeleteParameterResponse
    -- * Response Lenses
    , dpprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteParameter' smart constructor.
newtype DeleteParameter = DeleteParameter'
  { _delName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delName' - The name of the parameter to delete.
deleteParameter
    :: Text -- ^ 'delName'
    -> DeleteParameter
deleteParameter pName_ = DeleteParameter' {_delName = pName_}


-- | The name of the parameter to delete.
delName :: Lens' DeleteParameter Text
delName = lens _delName (\ s a -> s{_delName = a})

instance AWSRequest DeleteParameter where
        type Rs DeleteParameter = DeleteParameterResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteParameterResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteParameter where

instance NFData DeleteParameter where

instance ToHeaders DeleteParameter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteParameter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteParameter where
        toJSON DeleteParameter'{..}
          = object (catMaybes [Just ("Name" .= _delName)])

instance ToPath DeleteParameter where
        toPath = const "/"

instance ToQuery DeleteParameter where
        toQuery = const mempty

-- | /See:/ 'deleteParameterResponse' smart constructor.
newtype DeleteParameterResponse = DeleteParameterResponse'
  { _dpprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteParameterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpprsResponseStatus' - -- | The response status code.
deleteParameterResponse
    :: Int -- ^ 'dpprsResponseStatus'
    -> DeleteParameterResponse
deleteParameterResponse pResponseStatus_ =
  DeleteParameterResponse' {_dpprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dpprsResponseStatus :: Lens' DeleteParameterResponse Int
dpprsResponseStatus = lens _dpprsResponseStatus (\ s a -> s{_dpprsResponseStatus = a})

instance NFData DeleteParameterResponse where
