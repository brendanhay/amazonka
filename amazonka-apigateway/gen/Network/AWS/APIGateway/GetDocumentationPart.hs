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
-- Module      : Network.AWS.APIGateway.GetDocumentationPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationPart
    (
    -- * Creating a Request
      getDocumentationPart
    , GetDocumentationPart
    -- * Request Lenses
    , getRestAPIId
    , getDocumentationPartId

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

-- | Gets a specified documentation part of a given API.
--
--
--
-- /See:/ 'getDocumentationPart' smart constructor.
data GetDocumentationPart = GetDocumentationPart'
  { _getRestAPIId           :: !Text
  , _getDocumentationPartId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDocumentationPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'getDocumentationPartId' - [Required] The string identifier of the associated 'RestApi' .
getDocumentationPart
    :: Text -- ^ 'getRestAPIId'
    -> Text -- ^ 'getDocumentationPartId'
    -> GetDocumentationPart
getDocumentationPart pRestAPIId_ pDocumentationPartId_ =
  GetDocumentationPart'
    { _getRestAPIId = pRestAPIId_
    , _getDocumentationPartId = pDocumentationPartId_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
getRestAPIId :: Lens' GetDocumentationPart Text
getRestAPIId = lens _getRestAPIId (\ s a -> s{_getRestAPIId = a})

-- | [Required] The string identifier of the associated 'RestApi' .
getDocumentationPartId :: Lens' GetDocumentationPart Text
getDocumentationPartId = lens _getDocumentationPartId (\ s a -> s{_getDocumentationPartId = a})

instance AWSRequest GetDocumentationPart where
        type Rs GetDocumentationPart = DocumentationPart
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetDocumentationPart where

instance NFData GetDocumentationPart where

instance ToHeaders GetDocumentationPart where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDocumentationPart where
        toPath GetDocumentationPart'{..}
          = mconcat
              ["/restapis/", toBS _getRestAPIId,
               "/documentation/parts/",
               toBS _getDocumentationPartId]

instance ToQuery GetDocumentationPart where
        toQuery = const mempty
