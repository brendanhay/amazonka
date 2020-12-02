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
-- Module      : Network.AWS.APIGateway.DeleteIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration.
--
--
module Network.AWS.APIGateway.DeleteIntegration
    (
    -- * Creating a Request
      deleteIntegration
    , DeleteIntegration
    -- * Request Lenses
    , delRestAPIId
    , delResourceId
    , delHttpMethod

    -- * Destructuring the Response
    , deleteIntegrationResponse'
    , DeleteIntegrationResponse'
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a delete integration request.
--
--
--
-- /See:/ 'deleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { _delRestAPIId  :: !Text
  , _delResourceId :: !Text
  , _delHttpMethod :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'delResourceId' - [Required] Specifies a delete integration request's resource identifier.
--
-- * 'delHttpMethod' - [Required] Specifies a delete integration request's HTTP method.
deleteIntegration
    :: Text -- ^ 'delRestAPIId'
    -> Text -- ^ 'delResourceId'
    -> Text -- ^ 'delHttpMethod'
    -> DeleteIntegration
deleteIntegration pRestAPIId_ pResourceId_ pHttpMethod_ =
  DeleteIntegration'
    { _delRestAPIId = pRestAPIId_
    , _delResourceId = pResourceId_
    , _delHttpMethod = pHttpMethod_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
delRestAPIId :: Lens' DeleteIntegration Text
delRestAPIId = lens _delRestAPIId (\ s a -> s{_delRestAPIId = a})

-- | [Required] Specifies a delete integration request's resource identifier.
delResourceId :: Lens' DeleteIntegration Text
delResourceId = lens _delResourceId (\ s a -> s{_delResourceId = a})

-- | [Required] Specifies a delete integration request's HTTP method.
delHttpMethod :: Lens' DeleteIntegration Text
delHttpMethod = lens _delHttpMethod (\ s a -> s{_delHttpMethod = a})

instance AWSRequest DeleteIntegration where
        type Rs DeleteIntegration =
             DeleteIntegrationResponse'
        request = delete apiGateway
        response = receiveNull DeleteIntegrationResponse''

instance Hashable DeleteIntegration where

instance NFData DeleteIntegration where

instance ToHeaders DeleteIntegration where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteIntegration where
        toPath DeleteIntegration'{..}
          = mconcat
              ["/restapis/", toBS _delRestAPIId, "/resources/",
               toBS _delResourceId, "/methods/",
               toBS _delHttpMethod, "/integration"]

instance ToQuery DeleteIntegration where
        toQuery = const mempty

-- | /See:/ 'deleteIntegrationResponse'' smart constructor.
data DeleteIntegrationResponse' =
  DeleteIntegrationResponse''
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegrationResponse'' with the minimum fields required to make a request.
--
deleteIntegrationResponse'
    :: DeleteIntegrationResponse'
deleteIntegrationResponse' = DeleteIntegrationResponse''


instance NFData DeleteIntegrationResponse' where
