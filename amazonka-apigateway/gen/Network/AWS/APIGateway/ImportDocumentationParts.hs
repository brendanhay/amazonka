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
-- Module      : Network.AWS.APIGateway.ImportDocumentationParts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.ImportDocumentationParts
    (
    -- * Creating a Request
      importDocumentationParts
    , ImportDocumentationParts
    -- * Request Lenses
    , idpMode
    , idpFailOnWarnings
    , idpRestAPIId
    , idpBody

    -- * Destructuring the Response
    , importDocumentationPartsResponse
    , ImportDocumentationPartsResponse
    -- * Response Lenses
    , idprsIds
    , idprsWarnings
    , idprsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Import documentation parts from an external (e.g., Swagger) definition file.
--
--
--
-- /See:/ 'importDocumentationParts' smart constructor.
data ImportDocumentationParts = ImportDocumentationParts'
  { _idpMode           :: !(Maybe PutMode)
  , _idpFailOnWarnings :: !(Maybe Bool)
  , _idpRestAPIId      :: !Text
  , _idpBody           :: !ByteString
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportDocumentationParts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idpMode' - A query parameter to indicate whether to overwrite (@OVERWRITE@ ) any existing 'DocumentationParts' definition or to merge (@MERGE@ ) the new definition into the existing one. The default value is @MERGE@ .
--
-- * 'idpFailOnWarnings' - A query parameter to specify whether to rollback the documentation importation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- * 'idpRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'idpBody' - [Required] Raw byte array representing the to-be-imported documentation parts. To import from a Swagger file, this is a JSON object.
importDocumentationParts
    :: Text -- ^ 'idpRestAPIId'
    -> ByteString -- ^ 'idpBody'
    -> ImportDocumentationParts
importDocumentationParts pRestAPIId_ pBody_ =
  ImportDocumentationParts'
    { _idpMode = Nothing
    , _idpFailOnWarnings = Nothing
    , _idpRestAPIId = pRestAPIId_
    , _idpBody = pBody_
    }


-- | A query parameter to indicate whether to overwrite (@OVERWRITE@ ) any existing 'DocumentationParts' definition or to merge (@MERGE@ ) the new definition into the existing one. The default value is @MERGE@ .
idpMode :: Lens' ImportDocumentationParts (Maybe PutMode)
idpMode = lens _idpMode (\ s a -> s{_idpMode = a})

-- | A query parameter to specify whether to rollback the documentation importation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
idpFailOnWarnings :: Lens' ImportDocumentationParts (Maybe Bool)
idpFailOnWarnings = lens _idpFailOnWarnings (\ s a -> s{_idpFailOnWarnings = a})

-- | [Required] The string identifier of the associated 'RestApi' .
idpRestAPIId :: Lens' ImportDocumentationParts Text
idpRestAPIId = lens _idpRestAPIId (\ s a -> s{_idpRestAPIId = a})

-- | [Required] Raw byte array representing the to-be-imported documentation parts. To import from a Swagger file, this is a JSON object.
idpBody :: Lens' ImportDocumentationParts ByteString
idpBody = lens _idpBody (\ s a -> s{_idpBody = a})

instance AWSRequest ImportDocumentationParts where
        type Rs ImportDocumentationParts =
             ImportDocumentationPartsResponse
        request = putBody apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 ImportDocumentationPartsResponse' <$>
                   (x .?> "ids" .!@ mempty) <*>
                     (x .?> "warnings" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ImportDocumentationParts where

instance NFData ImportDocumentationParts where

instance ToBody ImportDocumentationParts where
        toBody = toBody . _idpBody

instance ToHeaders ImportDocumentationParts where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath ImportDocumentationParts where
        toPath ImportDocumentationParts'{..}
          = mconcat
              ["/restapis/", toBS _idpRestAPIId,
               "/documentation/parts"]

instance ToQuery ImportDocumentationParts where
        toQuery ImportDocumentationParts'{..}
          = mconcat
              ["mode" =: _idpMode,
               "failonwarnings" =: _idpFailOnWarnings]

-- | A collection of the imported 'DocumentationPart' identifiers.
--
--
-- This is used to return the result when documentation parts in an external (e.g., Swagger) file are imported into API Gateway<http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , <http://docs.aws.amazon.com/apigateway/api-reference/link-relation/documentationpart-import/ documentationpart:import> , 'DocumentationPart'
--
-- /See:/ 'importDocumentationPartsResponse' smart constructor.
data ImportDocumentationPartsResponse = ImportDocumentationPartsResponse'
  { _idprsIds            :: !(Maybe [Text])
  , _idprsWarnings       :: !(Maybe [Text])
  , _idprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportDocumentationPartsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idprsIds' - A list of the returned documentation part identifiers.
--
-- * 'idprsWarnings' - A list of warning messages reported during import of documentation parts.
--
-- * 'idprsResponseStatus' - -- | The response status code.
importDocumentationPartsResponse
    :: Int -- ^ 'idprsResponseStatus'
    -> ImportDocumentationPartsResponse
importDocumentationPartsResponse pResponseStatus_ =
  ImportDocumentationPartsResponse'
    { _idprsIds = Nothing
    , _idprsWarnings = Nothing
    , _idprsResponseStatus = pResponseStatus_
    }


-- | A list of the returned documentation part identifiers.
idprsIds :: Lens' ImportDocumentationPartsResponse [Text]
idprsIds = lens _idprsIds (\ s a -> s{_idprsIds = a}) . _Default . _Coerce

-- | A list of warning messages reported during import of documentation parts.
idprsWarnings :: Lens' ImportDocumentationPartsResponse [Text]
idprsWarnings = lens _idprsWarnings (\ s a -> s{_idprsWarnings = a}) . _Default . _Coerce

-- | -- | The response status code.
idprsResponseStatus :: Lens' ImportDocumentationPartsResponse Int
idprsResponseStatus = lens _idprsResponseStatus (\ s a -> s{_idprsResponseStatus = a})

instance NFData ImportDocumentationPartsResponse
         where
