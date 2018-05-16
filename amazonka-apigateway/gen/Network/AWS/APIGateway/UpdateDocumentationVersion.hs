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
-- Module      : Network.AWS.APIGateway.UpdateDocumentationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.UpdateDocumentationVersion
    (
    -- * Creating a Request
      updateDocumentationVersion
    , UpdateDocumentationVersion
    -- * Request Lenses
    , udvPatchOperations
    , udvRestAPIId
    , udvDocumentationVersion

    -- * Destructuring the Response
    , documentationVersion
    , DocumentationVersion
    -- * Response Lenses
    , dvCreatedDate
    , dvVersion
    , dvDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates an existing documentation version of an API.
--
--
--
-- /See:/ 'updateDocumentationVersion' smart constructor.
data UpdateDocumentationVersion = UpdateDocumentationVersion'
  { _udvPatchOperations      :: !(Maybe [PatchOperation])
  , _udvRestAPIId            :: !Text
  , _udvDocumentationVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDocumentationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udvPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'udvRestAPIId' - [Required] The string identifier of the associated 'RestApi' ..
--
-- * 'udvDocumentationVersion' - [Required] The version identifier of the to-be-updated documentation version.
updateDocumentationVersion
    :: Text -- ^ 'udvRestAPIId'
    -> Text -- ^ 'udvDocumentationVersion'
    -> UpdateDocumentationVersion
updateDocumentationVersion pRestAPIId_ pDocumentationVersion_ =
  UpdateDocumentationVersion'
    { _udvPatchOperations = Nothing
    , _udvRestAPIId = pRestAPIId_
    , _udvDocumentationVersion = pDocumentationVersion_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
udvPatchOperations :: Lens' UpdateDocumentationVersion [PatchOperation]
udvPatchOperations = lens _udvPatchOperations (\ s a -> s{_udvPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' ..
udvRestAPIId :: Lens' UpdateDocumentationVersion Text
udvRestAPIId = lens _udvRestAPIId (\ s a -> s{_udvRestAPIId = a})

-- | [Required] The version identifier of the to-be-updated documentation version.
udvDocumentationVersion :: Lens' UpdateDocumentationVersion Text
udvDocumentationVersion = lens _udvDocumentationVersion (\ s a -> s{_udvDocumentationVersion = a})

instance AWSRequest UpdateDocumentationVersion where
        type Rs UpdateDocumentationVersion =
             DocumentationVersion
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateDocumentationVersion where

instance NFData UpdateDocumentationVersion where

instance ToHeaders UpdateDocumentationVersion where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateDocumentationVersion where
        toJSON UpdateDocumentationVersion'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _udvPatchOperations])

instance ToPath UpdateDocumentationVersion where
        toPath UpdateDocumentationVersion'{..}
          = mconcat
              ["/restapis/", toBS _udvRestAPIId,
               "/documentation/versions/",
               toBS _udvDocumentationVersion]

instance ToQuery UpdateDocumentationVersion where
        toQuery = const mempty
