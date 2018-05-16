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
-- Module      : Network.AWS.APIGateway.DeleteResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Resource' resource.
--
--
module Network.AWS.APIGateway.DeleteResource
    (
    -- * Creating a Request
      deleteResource
    , DeleteResource
    -- * Request Lenses
    , drRestAPIId
    , drResourceId

    -- * Destructuring the Response
    , deleteResourceResponse
    , DeleteResourceResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete a 'Resource' .
--
--
--
-- /See:/ 'deleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { _drRestAPIId  :: !Text
  , _drResourceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'drResourceId' - [Required] The identifier of the 'Resource' resource.
deleteResource
    :: Text -- ^ 'drRestAPIId'
    -> Text -- ^ 'drResourceId'
    -> DeleteResource
deleteResource pRestAPIId_ pResourceId_ =
  DeleteResource' {_drRestAPIId = pRestAPIId_, _drResourceId = pResourceId_}


-- | [Required] The string identifier of the associated 'RestApi' .
drRestAPIId :: Lens' DeleteResource Text
drRestAPIId = lens _drRestAPIId (\ s a -> s{_drRestAPIId = a})

-- | [Required] The identifier of the 'Resource' resource.
drResourceId :: Lens' DeleteResource Text
drResourceId = lens _drResourceId (\ s a -> s{_drResourceId = a})

instance AWSRequest DeleteResource where
        type Rs DeleteResource = DeleteResourceResponse
        request = delete apiGateway
        response = receiveNull DeleteResourceResponse'

instance Hashable DeleteResource where

instance NFData DeleteResource where

instance ToHeaders DeleteResource where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteResource where
        toPath DeleteResource'{..}
          = mconcat
              ["/restapis/", toBS _drRestAPIId, "/resources/",
               toBS _drResourceId]

instance ToQuery DeleteResource where
        toQuery = const mempty

-- | /See:/ 'deleteResourceResponse' smart constructor.
data DeleteResourceResponse =
  DeleteResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourceResponse' with the minimum fields required to make a request.
--
deleteResourceResponse
    :: DeleteResourceResponse
deleteResourceResponse = DeleteResourceResponse'


instance NFData DeleteResourceResponse where
