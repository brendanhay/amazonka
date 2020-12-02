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
-- Module      : Network.AWS.APIGateway.DeleteDocumentationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.DeleteDocumentationVersion
    (
    -- * Creating a Request
      deleteDocumentationVersion
    , DeleteDocumentationVersion
    -- * Request Lenses
    , ddvRestAPIId
    , ddvDocumentationVersion

    -- * Destructuring the Response
    , deleteDocumentationVersionResponse
    , DeleteDocumentationVersionResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes an existing documentation version of an API.
--
--
--
-- /See:/ 'deleteDocumentationVersion' smart constructor.
data DeleteDocumentationVersion = DeleteDocumentationVersion'
  { _ddvRestAPIId            :: !Text
  , _ddvDocumentationVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'ddvDocumentationVersion' - [Required] The version identifier of a to-be-deleted documentation snapshot.
deleteDocumentationVersion
    :: Text -- ^ 'ddvRestAPIId'
    -> Text -- ^ 'ddvDocumentationVersion'
    -> DeleteDocumentationVersion
deleteDocumentationVersion pRestAPIId_ pDocumentationVersion_ =
  DeleteDocumentationVersion'
    { _ddvRestAPIId = pRestAPIId_
    , _ddvDocumentationVersion = pDocumentationVersion_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
ddvRestAPIId :: Lens' DeleteDocumentationVersion Text
ddvRestAPIId = lens _ddvRestAPIId (\ s a -> s{_ddvRestAPIId = a})

-- | [Required] The version identifier of a to-be-deleted documentation snapshot.
ddvDocumentationVersion :: Lens' DeleteDocumentationVersion Text
ddvDocumentationVersion = lens _ddvDocumentationVersion (\ s a -> s{_ddvDocumentationVersion = a})

instance AWSRequest DeleteDocumentationVersion where
        type Rs DeleteDocumentationVersion =
             DeleteDocumentationVersionResponse
        request = delete apiGateway
        response
          = receiveNull DeleteDocumentationVersionResponse'

instance Hashable DeleteDocumentationVersion where

instance NFData DeleteDocumentationVersion where

instance ToHeaders DeleteDocumentationVersion where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteDocumentationVersion where
        toPath DeleteDocumentationVersion'{..}
          = mconcat
              ["/restapis/", toBS _ddvRestAPIId,
               "/documentation/versions/",
               toBS _ddvDocumentationVersion]

instance ToQuery DeleteDocumentationVersion where
        toQuery = const mempty

-- | /See:/ 'deleteDocumentationVersionResponse' smart constructor.
data DeleteDocumentationVersionResponse =
  DeleteDocumentationVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDocumentationVersionResponse' with the minimum fields required to make a request.
--
deleteDocumentationVersionResponse
    :: DeleteDocumentationVersionResponse
deleteDocumentationVersionResponse = DeleteDocumentationVersionResponse'


instance NFData DeleteDocumentationVersionResponse
         where
