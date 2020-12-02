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
-- Module      : Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate the specified TagOption with the specified portfolio or product.
--
--
module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
    (
    -- * Creating a Request
      associateTagOptionWithResource
    , AssociateTagOptionWithResource
    -- * Request Lenses
    , atowrResourceId
    , atowrTagOptionId

    -- * Destructuring the Response
    , associateTagOptionWithResourceResponse
    , AssociateTagOptionWithResourceResponse
    -- * Response Lenses
    , atowrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'associateTagOptionWithResource' smart constructor.
data AssociateTagOptionWithResource = AssociateTagOptionWithResource'
  { _atowrResourceId  :: !Text
  , _atowrTagOptionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateTagOptionWithResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atowrResourceId' - The resource identifier.
--
-- * 'atowrTagOptionId' - The TagOption identifier.
associateTagOptionWithResource
    :: Text -- ^ 'atowrResourceId'
    -> Text -- ^ 'atowrTagOptionId'
    -> AssociateTagOptionWithResource
associateTagOptionWithResource pResourceId_ pTagOptionId_ =
  AssociateTagOptionWithResource'
    {_atowrResourceId = pResourceId_, _atowrTagOptionId = pTagOptionId_}


-- | The resource identifier.
atowrResourceId :: Lens' AssociateTagOptionWithResource Text
atowrResourceId = lens _atowrResourceId (\ s a -> s{_atowrResourceId = a})

-- | The TagOption identifier.
atowrTagOptionId :: Lens' AssociateTagOptionWithResource Text
atowrTagOptionId = lens _atowrTagOptionId (\ s a -> s{_atowrTagOptionId = a})

instance AWSRequest AssociateTagOptionWithResource
         where
        type Rs AssociateTagOptionWithResource =
             AssociateTagOptionWithResourceResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateTagOptionWithResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateTagOptionWithResource
         where

instance NFData AssociateTagOptionWithResource where

instance ToHeaders AssociateTagOptionWithResource
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.AssociateTagOptionWithResource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateTagOptionWithResource where
        toJSON AssociateTagOptionWithResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _atowrResourceId),
                  Just ("TagOptionId" .= _atowrTagOptionId)])

instance ToPath AssociateTagOptionWithResource where
        toPath = const "/"

instance ToQuery AssociateTagOptionWithResource where
        toQuery = const mempty

-- | /See:/ 'associateTagOptionWithResourceResponse' smart constructor.
newtype AssociateTagOptionWithResourceResponse = AssociateTagOptionWithResourceResponse'
  { _atowrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateTagOptionWithResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atowrrsResponseStatus' - -- | The response status code.
associateTagOptionWithResourceResponse
    :: Int -- ^ 'atowrrsResponseStatus'
    -> AssociateTagOptionWithResourceResponse
associateTagOptionWithResourceResponse pResponseStatus_ =
  AssociateTagOptionWithResourceResponse'
    {_atowrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
atowrrsResponseStatus :: Lens' AssociateTagOptionWithResourceResponse Int
atowrrsResponseStatus = lens _atowrrsResponseStatus (\ s a -> s{_atowrrsResponseStatus = a})

instance NFData
           AssociateTagOptionWithResourceResponse
         where
