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
-- Module      : Network.AWS.ServiceCatalog.UpdateTagOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified TagOption.
--
--
module Network.AWS.ServiceCatalog.UpdateTagOption
    (
    -- * Creating a Request
      updateTagOption
    , UpdateTagOption
    -- * Request Lenses
    , utoValue
    , utoActive
    , utoId

    -- * Destructuring the Response
    , updateTagOptionResponse
    , UpdateTagOptionResponse
    -- * Response Lenses
    , utorsTagOptionDetail
    , utorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'updateTagOption' smart constructor.
data UpdateTagOption = UpdateTagOption'
  { _utoValue  :: !(Maybe Text)
  , _utoActive :: !(Maybe Bool)
  , _utoId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTagOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utoValue' - The updated value.
--
-- * 'utoActive' - The updated active state.
--
-- * 'utoId' - The TagOption identifier.
updateTagOption
    :: Text -- ^ 'utoId'
    -> UpdateTagOption
updateTagOption pId_ =
  UpdateTagOption' {_utoValue = Nothing, _utoActive = Nothing, _utoId = pId_}


-- | The updated value.
utoValue :: Lens' UpdateTagOption (Maybe Text)
utoValue = lens _utoValue (\ s a -> s{_utoValue = a})

-- | The updated active state.
utoActive :: Lens' UpdateTagOption (Maybe Bool)
utoActive = lens _utoActive (\ s a -> s{_utoActive = a})

-- | The TagOption identifier.
utoId :: Lens' UpdateTagOption Text
utoId = lens _utoId (\ s a -> s{_utoId = a})

instance AWSRequest UpdateTagOption where
        type Rs UpdateTagOption = UpdateTagOptionResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTagOptionResponse' <$>
                   (x .?> "TagOptionDetail") <*> (pure (fromEnum s)))

instance Hashable UpdateTagOption where

instance NFData UpdateTagOption where

instance ToHeaders UpdateTagOption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.UpdateTagOption" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTagOption where
        toJSON UpdateTagOption'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _utoValue,
                  ("Active" .=) <$> _utoActive, Just ("Id" .= _utoId)])

instance ToPath UpdateTagOption where
        toPath = const "/"

instance ToQuery UpdateTagOption where
        toQuery = const mempty

-- | /See:/ 'updateTagOptionResponse' smart constructor.
data UpdateTagOptionResponse = UpdateTagOptionResponse'
  { _utorsTagOptionDetail :: !(Maybe TagOptionDetail)
  , _utorsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTagOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utorsTagOptionDetail' - Information about the TagOption.
--
-- * 'utorsResponseStatus' - -- | The response status code.
updateTagOptionResponse
    :: Int -- ^ 'utorsResponseStatus'
    -> UpdateTagOptionResponse
updateTagOptionResponse pResponseStatus_ =
  UpdateTagOptionResponse'
    {_utorsTagOptionDetail = Nothing, _utorsResponseStatus = pResponseStatus_}


-- | Information about the TagOption.
utorsTagOptionDetail :: Lens' UpdateTagOptionResponse (Maybe TagOptionDetail)
utorsTagOptionDetail = lens _utorsTagOptionDetail (\ s a -> s{_utorsTagOptionDetail = a})

-- | -- | The response status code.
utorsResponseStatus :: Lens' UpdateTagOptionResponse Int
utorsResponseStatus = lens _utorsResponseStatus (\ s a -> s{_utorsResponseStatus = a})

instance NFData UpdateTagOptionResponse where
