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
-- Module      : Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified TagOption from the specified resource.
--
--
module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
    (
    -- * Creating a Request
      disassociateTagOptionFromResource
    , DisassociateTagOptionFromResource
    -- * Request Lenses
    , dtofrResourceId
    , dtofrTagOptionId

    -- * Destructuring the Response
    , disassociateTagOptionFromResourceResponse
    , DisassociateTagOptionFromResourceResponse
    -- * Response Lenses
    , dtofrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'disassociateTagOptionFromResource' smart constructor.
data DisassociateTagOptionFromResource = DisassociateTagOptionFromResource'
  { _dtofrResourceId  :: !Text
  , _dtofrTagOptionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateTagOptionFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtofrResourceId' - The resource identifier.
--
-- * 'dtofrTagOptionId' - The TagOption identifier.
disassociateTagOptionFromResource
    :: Text -- ^ 'dtofrResourceId'
    -> Text -- ^ 'dtofrTagOptionId'
    -> DisassociateTagOptionFromResource
disassociateTagOptionFromResource pResourceId_ pTagOptionId_ =
  DisassociateTagOptionFromResource'
    {_dtofrResourceId = pResourceId_, _dtofrTagOptionId = pTagOptionId_}


-- | The resource identifier.
dtofrResourceId :: Lens' DisassociateTagOptionFromResource Text
dtofrResourceId = lens _dtofrResourceId (\ s a -> s{_dtofrResourceId = a})

-- | The TagOption identifier.
dtofrTagOptionId :: Lens' DisassociateTagOptionFromResource Text
dtofrTagOptionId = lens _dtofrTagOptionId (\ s a -> s{_dtofrTagOptionId = a})

instance AWSRequest DisassociateTagOptionFromResource
         where
        type Rs DisassociateTagOptionFromResource =
             DisassociateTagOptionFromResourceResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateTagOptionFromResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateTagOptionFromResource
         where

instance NFData DisassociateTagOptionFromResource
         where

instance ToHeaders DisassociateTagOptionFromResource
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DisassociateTagOptionFromResource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateTagOptionFromResource
         where
        toJSON DisassociateTagOptionFromResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _dtofrResourceId),
                  Just ("TagOptionId" .= _dtofrTagOptionId)])

instance ToPath DisassociateTagOptionFromResource
         where
        toPath = const "/"

instance ToQuery DisassociateTagOptionFromResource
         where
        toQuery = const mempty

-- | /See:/ 'disassociateTagOptionFromResourceResponse' smart constructor.
newtype DisassociateTagOptionFromResourceResponse = DisassociateTagOptionFromResourceResponse'
  { _dtofrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateTagOptionFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtofrrsResponseStatus' - -- | The response status code.
disassociateTagOptionFromResourceResponse
    :: Int -- ^ 'dtofrrsResponseStatus'
    -> DisassociateTagOptionFromResourceResponse
disassociateTagOptionFromResourceResponse pResponseStatus_ =
  DisassociateTagOptionFromResourceResponse'
    {_dtofrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtofrrsResponseStatus :: Lens' DisassociateTagOptionFromResourceResponse Int
dtofrrsResponseStatus = lens _dtofrrsResponseStatus (\ s a -> s{_dtofrrsResponseStatus = a})

instance NFData
           DisassociateTagOptionFromResourceResponse
         where
