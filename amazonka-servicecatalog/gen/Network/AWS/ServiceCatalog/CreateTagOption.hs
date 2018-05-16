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
-- Module      : Network.AWS.ServiceCatalog.CreateTagOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a TagOption.
--
--
module Network.AWS.ServiceCatalog.CreateTagOption
    (
    -- * Creating a Request
      createTagOption
    , CreateTagOption
    -- * Request Lenses
    , ctoKey
    , ctoValue

    -- * Destructuring the Response
    , createTagOptionResponse
    , CreateTagOptionResponse
    -- * Response Lenses
    , ctorsTagOptionDetail
    , ctorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createTagOption' smart constructor.
data CreateTagOption = CreateTagOption'
  { _ctoKey   :: !Text
  , _ctoValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctoKey' - The TagOption key.
--
-- * 'ctoValue' - The TagOption value.
createTagOption
    :: Text -- ^ 'ctoKey'
    -> Text -- ^ 'ctoValue'
    -> CreateTagOption
createTagOption pKey_ pValue_ =
  CreateTagOption' {_ctoKey = pKey_, _ctoValue = pValue_}


-- | The TagOption key.
ctoKey :: Lens' CreateTagOption Text
ctoKey = lens _ctoKey (\ s a -> s{_ctoKey = a})

-- | The TagOption value.
ctoValue :: Lens' CreateTagOption Text
ctoValue = lens _ctoValue (\ s a -> s{_ctoValue = a})

instance AWSRequest CreateTagOption where
        type Rs CreateTagOption = CreateTagOptionResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 CreateTagOptionResponse' <$>
                   (x .?> "TagOptionDetail") <*> (pure (fromEnum s)))

instance Hashable CreateTagOption where

instance NFData CreateTagOption where

instance ToHeaders CreateTagOption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreateTagOption" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTagOption where
        toJSON CreateTagOption'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _ctoKey),
                  Just ("Value" .= _ctoValue)])

instance ToPath CreateTagOption where
        toPath = const "/"

instance ToQuery CreateTagOption where
        toQuery = const mempty

-- | /See:/ 'createTagOptionResponse' smart constructor.
data CreateTagOptionResponse = CreateTagOptionResponse'
  { _ctorsTagOptionDetail :: !(Maybe TagOptionDetail)
  , _ctorsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctorsTagOptionDetail' - Information about the TagOption.
--
-- * 'ctorsResponseStatus' - -- | The response status code.
createTagOptionResponse
    :: Int -- ^ 'ctorsResponseStatus'
    -> CreateTagOptionResponse
createTagOptionResponse pResponseStatus_ =
  CreateTagOptionResponse'
    {_ctorsTagOptionDetail = Nothing, _ctorsResponseStatus = pResponseStatus_}


-- | Information about the TagOption.
ctorsTagOptionDetail :: Lens' CreateTagOptionResponse (Maybe TagOptionDetail)
ctorsTagOptionDetail = lens _ctorsTagOptionDetail (\ s a -> s{_ctorsTagOptionDetail = a})

-- | -- | The response status code.
ctorsResponseStatus :: Lens' CreateTagOptionResponse Int
ctorsResponseStatus = lens _ctorsResponseStatus (\ s a -> s{_ctorsResponseStatus = a})

instance NFData CreateTagOptionResponse where
