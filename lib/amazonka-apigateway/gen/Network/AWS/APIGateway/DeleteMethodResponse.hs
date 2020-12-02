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
-- Module      : Network.AWS.APIGateway.DeleteMethodResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'MethodResponse' resource.
--
--
module Network.AWS.APIGateway.DeleteMethodResponse
    (
    -- * Creating a Request
      deleteMethodResponse
    , DeleteMethodResponse
    -- * Request Lenses
    , dmRestAPIId
    , dmResourceId
    , dmHttpMethod
    , dmStatusCode

    -- * Destructuring the Response
    , deleteMethodResponseResponse
    , DeleteMethodResponseResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to delete an existing 'MethodResponse' resource.
--
--
--
-- /See:/ 'deleteMethodResponse' smart constructor.
data DeleteMethodResponse = DeleteMethodResponse'
  { _dmRestAPIId  :: !Text
  , _dmResourceId :: !Text
  , _dmHttpMethod :: !Text
  , _dmStatusCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'dmResourceId' - [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- * 'dmHttpMethod' - [Required] The HTTP verb of the 'Method' resource.
--
-- * 'dmStatusCode' - [Required] The status code identifier for the 'MethodResponse' resource.
deleteMethodResponse
    :: Text -- ^ 'dmRestAPIId'
    -> Text -- ^ 'dmResourceId'
    -> Text -- ^ 'dmHttpMethod'
    -> Text -- ^ 'dmStatusCode'
    -> DeleteMethodResponse
deleteMethodResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
  DeleteMethodResponse'
    { _dmRestAPIId = pRestAPIId_
    , _dmResourceId = pResourceId_
    , _dmHttpMethod = pHttpMethod_
    , _dmStatusCode = pStatusCode_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
dmRestAPIId :: Lens' DeleteMethodResponse Text
dmRestAPIId = lens _dmRestAPIId (\ s a -> s{_dmRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
dmResourceId :: Lens' DeleteMethodResponse Text
dmResourceId = lens _dmResourceId (\ s a -> s{_dmResourceId = a})

-- | [Required] The HTTP verb of the 'Method' resource.
dmHttpMethod :: Lens' DeleteMethodResponse Text
dmHttpMethod = lens _dmHttpMethod (\ s a -> s{_dmHttpMethod = a})

-- | [Required] The status code identifier for the 'MethodResponse' resource.
dmStatusCode :: Lens' DeleteMethodResponse Text
dmStatusCode = lens _dmStatusCode (\ s a -> s{_dmStatusCode = a})

instance AWSRequest DeleteMethodResponse where
        type Rs DeleteMethodResponse =
             DeleteMethodResponseResponse
        request = delete apiGateway
        response = receiveNull DeleteMethodResponseResponse'

instance Hashable DeleteMethodResponse where

instance NFData DeleteMethodResponse where

instance ToHeaders DeleteMethodResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteMethodResponse where
        toPath DeleteMethodResponse'{..}
          = mconcat
              ["/restapis/", toBS _dmRestAPIId, "/resources/",
               toBS _dmResourceId, "/methods/", toBS _dmHttpMethod,
               "/responses/", toBS _dmStatusCode]

instance ToQuery DeleteMethodResponse where
        toQuery = const mempty

-- | /See:/ 'deleteMethodResponseResponse' smart constructor.
data DeleteMethodResponseResponse =
  DeleteMethodResponseResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMethodResponseResponse' with the minimum fields required to make a request.
--
deleteMethodResponseResponse
    :: DeleteMethodResponseResponse
deleteMethodResponseResponse = DeleteMethodResponseResponse'


instance NFData DeleteMethodResponseResponse where
