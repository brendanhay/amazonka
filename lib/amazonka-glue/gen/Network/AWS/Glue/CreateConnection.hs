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
-- Module      : Network.AWS.Glue.CreateConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection definition in the Data Catalog.
--
--
module Network.AWS.Glue.CreateConnection
    (
    -- * Creating a Request
      createConnection
    , CreateConnection
    -- * Request Lenses
    , ccCatalogId
    , ccConnectionInput

    -- * Destructuring the Response
    , createConnectionResponse
    , CreateConnectionResponse
    -- * Response Lenses
    , crsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createConnection' smart constructor.
data CreateConnection = CreateConnection'
  { _ccCatalogId       :: !(Maybe Text)
  , _ccConnectionInput :: !ConnectionInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCatalogId' - The ID of the Data Catalog in which to create the connection. If none is supplied, the AWS account ID is used by default.
--
-- * 'ccConnectionInput' - A @ConnectionInput@ object defining the connection to create.
createConnection
    :: ConnectionInput -- ^ 'ccConnectionInput'
    -> CreateConnection
createConnection pConnectionInput_ =
  CreateConnection'
    {_ccCatalogId = Nothing, _ccConnectionInput = pConnectionInput_}


-- | The ID of the Data Catalog in which to create the connection. If none is supplied, the AWS account ID is used by default.
ccCatalogId :: Lens' CreateConnection (Maybe Text)
ccCatalogId = lens _ccCatalogId (\ s a -> s{_ccCatalogId = a})

-- | A @ConnectionInput@ object defining the connection to create.
ccConnectionInput :: Lens' CreateConnection ConnectionInput
ccConnectionInput = lens _ccConnectionInput (\ s a -> s{_ccConnectionInput = a})

instance AWSRequest CreateConnection where
        type Rs CreateConnection = CreateConnectionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreateConnectionResponse' <$> (pure (fromEnum s)))

instance Hashable CreateConnection where

instance NFData CreateConnection where

instance ToHeaders CreateConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateConnection where
        toJSON CreateConnection'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _ccCatalogId,
                  Just ("ConnectionInput" .= _ccConnectionInput)])

instance ToPath CreateConnection where
        toPath = const "/"

instance ToQuery CreateConnection where
        toQuery = const mempty

-- | /See:/ 'createConnectionResponse' smart constructor.
newtype CreateConnectionResponse = CreateConnectionResponse'
  { _crsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsResponseStatus' - -- | The response status code.
createConnectionResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateConnectionResponse
createConnectionResponse pResponseStatus_ =
  CreateConnectionResponse' {_crsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crsResponseStatus :: Lens' CreateConnectionResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateConnectionResponse where
