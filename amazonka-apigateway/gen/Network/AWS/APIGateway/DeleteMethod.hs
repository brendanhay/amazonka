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
-- Module      : Network.AWS.APIGateway.DeleteMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'Method' resource.
--
--
module Network.AWS.APIGateway.DeleteMethod
    (
    -- * Creating a Request
      deleteMethod
    , DeleteMethod
    -- * Request Lenses
    , dmmRestAPIId
    , dmmResourceId
    , dmmHttpMethod

    -- * Destructuring the Response
    , deleteMethodResponse'
    , DeleteMethodResponse'
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete an existing 'Method' resource.
--
--
--
-- /See:/ 'deleteMethod' smart constructor.
data DeleteMethod = DeleteMethod'
  { _dmmRestAPIId  :: !Text
  , _dmmResourceId :: !Text
  , _dmmHttpMethod :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmmRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'dmmResourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
--
-- * 'dmmHttpMethod' - [Required] The HTTP verb of the 'Method' resource.
deleteMethod
    :: Text -- ^ 'dmmRestAPIId'
    -> Text -- ^ 'dmmResourceId'
    -> Text -- ^ 'dmmHttpMethod'
    -> DeleteMethod
deleteMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
  DeleteMethod'
    { _dmmRestAPIId = pRestAPIId_
    , _dmmResourceId = pResourceId_
    , _dmmHttpMethod = pHttpMethod_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
dmmRestAPIId :: Lens' DeleteMethod Text
dmmRestAPIId = lens _dmmRestAPIId (\ s a -> s{_dmmRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'Method' resource.
dmmResourceId :: Lens' DeleteMethod Text
dmmResourceId = lens _dmmResourceId (\ s a -> s{_dmmResourceId = a})

-- | [Required] The HTTP verb of the 'Method' resource.
dmmHttpMethod :: Lens' DeleteMethod Text
dmmHttpMethod = lens _dmmHttpMethod (\ s a -> s{_dmmHttpMethod = a})

instance AWSRequest DeleteMethod where
        type Rs DeleteMethod = DeleteMethodResponse'
        request = delete apiGateway
        response = receiveNull DeleteMethodResponse''

instance Hashable DeleteMethod where

instance NFData DeleteMethod where

instance ToHeaders DeleteMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteMethod where
        toPath DeleteMethod'{..}
          = mconcat
              ["/restapis/", toBS _dmmRestAPIId, "/resources/",
               toBS _dmmResourceId, "/methods/",
               toBS _dmmHttpMethod]

instance ToQuery DeleteMethod where
        toQuery = const mempty

-- | /See:/ 'deleteMethodResponse'' smart constructor.
data DeleteMethodResponse' =
  DeleteMethodResponse''
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMethodResponse'' with the minimum fields required to make a request.
--
deleteMethodResponse'
    :: DeleteMethodResponse'
deleteMethodResponse' = DeleteMethodResponse''


instance NFData DeleteMethodResponse' where
