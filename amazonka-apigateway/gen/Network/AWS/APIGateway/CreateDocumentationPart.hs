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
-- Module      : Network.AWS.APIGateway.CreateDocumentationPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.CreateDocumentationPart
    (
    -- * Creating a Request
      createDocumentationPart
    , CreateDocumentationPart
    -- * Request Lenses
    , cdpRestAPIId
    , cdpLocation
    , cdpProperties

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

-- | Creates a new documentation part of a given API.
--
--
--
-- /See:/ 'createDocumentationPart' smart constructor.
data CreateDocumentationPart = CreateDocumentationPart'
  { _cdpRestAPIId  :: !Text
  , _cdpLocation   :: !DocumentationPartLocation
  , _cdpProperties :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDocumentationPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdpRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'cdpLocation' - [Required] The location of the targeted API entity of the to-be-created documentation part.
--
-- * 'cdpProperties' - [Required] The new documentation content map of the targeted API entity. Enclosed key-value pairs are API-specific, but only Swagger-compliant key-value pairs can be exported and, hence, published.
createDocumentationPart
    :: Text -- ^ 'cdpRestAPIId'
    -> DocumentationPartLocation -- ^ 'cdpLocation'
    -> Text -- ^ 'cdpProperties'
    -> CreateDocumentationPart
createDocumentationPart pRestAPIId_ pLocation_ pProperties_ =
  CreateDocumentationPart'
    { _cdpRestAPIId = pRestAPIId_
    , _cdpLocation = pLocation_
    , _cdpProperties = pProperties_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
cdpRestAPIId :: Lens' CreateDocumentationPart Text
cdpRestAPIId = lens _cdpRestAPIId (\ s a -> s{_cdpRestAPIId = a})

-- | [Required] The location of the targeted API entity of the to-be-created documentation part.
cdpLocation :: Lens' CreateDocumentationPart DocumentationPartLocation
cdpLocation = lens _cdpLocation (\ s a -> s{_cdpLocation = a})

-- | [Required] The new documentation content map of the targeted API entity. Enclosed key-value pairs are API-specific, but only Swagger-compliant key-value pairs can be exported and, hence, published.
cdpProperties :: Lens' CreateDocumentationPart Text
cdpProperties = lens _cdpProperties (\ s a -> s{_cdpProperties = a})

instance AWSRequest CreateDocumentationPart where
        type Rs CreateDocumentationPart = DocumentationPart
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateDocumentationPart where

instance NFData CreateDocumentationPart where

instance ToHeaders CreateDocumentationPart where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateDocumentationPart where
        toJSON CreateDocumentationPart'{..}
          = object
              (catMaybes
                 [Just ("location" .= _cdpLocation),
                  Just ("properties" .= _cdpProperties)])

instance ToPath CreateDocumentationPart where
        toPath CreateDocumentationPart'{..}
          = mconcat
              ["/restapis/", toBS _cdpRestAPIId,
               "/documentation/parts"]

instance ToQuery CreateDocumentationPart where
        toQuery = const mempty
