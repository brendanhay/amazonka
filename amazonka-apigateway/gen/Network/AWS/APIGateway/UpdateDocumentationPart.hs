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
-- Module      : Network.AWS.APIGateway.UpdateDocumentationPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.UpdateDocumentationPart
    (
    -- * Creating a Request
      updateDocumentationPart
    , UpdateDocumentationPart
    -- * Request Lenses
    , udpPatchOperations
    , udpRestAPIId
    , udpDocumentationPartId

    -- * Destructuring the Response
    , documentationPart
    , DocumentationPart
    -- * Response Lenses
    , dpLocation
    , dpId
    , dpProperties
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates an existing documentation part of a given API.
--
--
--
-- /See:/ 'updateDocumentationPart' smart constructor.
data UpdateDocumentationPart = UpdateDocumentationPart'
  { _udpPatchOperations     :: !(Maybe [PatchOperation])
  , _udpRestAPIId           :: !Text
  , _udpDocumentationPartId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentationPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udpPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'udpRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'udpDocumentationPartId' - [Required] The identifier of the to-be-updated documentation part.
updateDocumentationPart
    :: Text -- ^ 'udpRestAPIId'
    -> Text -- ^ 'udpDocumentationPartId'
    -> UpdateDocumentationPart
updateDocumentationPart pRestAPIId_ pDocumentationPartId_ =
  UpdateDocumentationPart'
    { _udpPatchOperations = Nothing
    , _udpRestAPIId = pRestAPIId_
    , _udpDocumentationPartId = pDocumentationPartId_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
udpPatchOperations :: Lens' UpdateDocumentationPart [PatchOperation]
udpPatchOperations = lens _udpPatchOperations (\ s a -> s{_udpPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
udpRestAPIId :: Lens' UpdateDocumentationPart Text
udpRestAPIId = lens _udpRestAPIId (\ s a -> s{_udpRestAPIId = a})

-- | [Required] The identifier of the to-be-updated documentation part.
udpDocumentationPartId :: Lens' UpdateDocumentationPart Text
udpDocumentationPartId = lens _udpDocumentationPartId (\ s a -> s{_udpDocumentationPartId = a})

instance AWSRequest UpdateDocumentationPart where
        type Rs UpdateDocumentationPart = DocumentationPart
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateDocumentationPart where

instance NFData UpdateDocumentationPart where

instance ToHeaders UpdateDocumentationPart where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateDocumentationPart where
        toJSON UpdateDocumentationPart'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _udpPatchOperations])

instance ToPath UpdateDocumentationPart where
        toPath UpdateDocumentationPart'{..}
          = mconcat
              ["/restapis/", toBS _udpRestAPIId,
               "/documentation/parts/",
               toBS _udpDocumentationPartId]

instance ToQuery UpdateDocumentationPart where
        toQuery = const mempty
