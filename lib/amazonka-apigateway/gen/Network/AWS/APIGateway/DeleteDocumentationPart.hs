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
-- Module      : Network.AWS.APIGateway.DeleteDocumentationPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.DeleteDocumentationPart
    (
    -- * Creating a Request
      deleteDocumentationPart
    , DeleteDocumentationPart
    -- * Request Lenses
    , ddpRestAPIId
    , ddpDocumentationPartId

    -- * Destructuring the Response
    , deleteDocumentationPartResponse
    , DeleteDocumentationPartResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes an existing documentation part of an API.
--
--
--
-- /See:/ 'deleteDocumentationPart' smart constructor.
data DeleteDocumentationPart = DeleteDocumentationPart'
  { _ddpRestAPIId           :: !Text
  , _ddpDocumentationPartId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentationPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'ddpDocumentationPartId' - [Required] The identifier of the to-be-deleted documentation part.
deleteDocumentationPart
    :: Text -- ^ 'ddpRestAPIId'
    -> Text -- ^ 'ddpDocumentationPartId'
    -> DeleteDocumentationPart
deleteDocumentationPart pRestAPIId_ pDocumentationPartId_ =
  DeleteDocumentationPart'
    { _ddpRestAPIId = pRestAPIId_
    , _ddpDocumentationPartId = pDocumentationPartId_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
ddpRestAPIId :: Lens' DeleteDocumentationPart Text
ddpRestAPIId = lens _ddpRestAPIId (\ s a -> s{_ddpRestAPIId = a})

-- | [Required] The identifier of the to-be-deleted documentation part.
ddpDocumentationPartId :: Lens' DeleteDocumentationPart Text
ddpDocumentationPartId = lens _ddpDocumentationPartId (\ s a -> s{_ddpDocumentationPartId = a})

instance AWSRequest DeleteDocumentationPart where
        type Rs DeleteDocumentationPart =
             DeleteDocumentationPartResponse
        request = delete apiGateway
        response
          = receiveNull DeleteDocumentationPartResponse'

instance Hashable DeleteDocumentationPart where

instance NFData DeleteDocumentationPart where

instance ToHeaders DeleteDocumentationPart where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteDocumentationPart where
        toPath DeleteDocumentationPart'{..}
          = mconcat
              ["/restapis/", toBS _ddpRestAPIId,
               "/documentation/parts/",
               toBS _ddpDocumentationPartId]

instance ToQuery DeleteDocumentationPart where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentationPartResponse' smart constructor.
data DeleteDocumentationPartResponse =
  DeleteDocumentationPartResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentationPartResponse' with the minimum fields required to make a request.
--
deleteDocumentationPartResponse
    :: DeleteDocumentationPartResponse
deleteDocumentationPartResponse = DeleteDocumentationPartResponse'


instance NFData DeleteDocumentationPartResponse where
