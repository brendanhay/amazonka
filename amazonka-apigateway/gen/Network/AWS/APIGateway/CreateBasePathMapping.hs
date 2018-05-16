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
-- Module      : Network.AWS.APIGateway.CreateBasePathMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'BasePathMapping' resource.
--
--
module Network.AWS.APIGateway.CreateBasePathMapping
    (
    -- * Creating a Request
      createBasePathMapping
    , CreateBasePathMapping
    -- * Request Lenses
    , cbpmStage
    , cbpmBasePath
    , cbpmDomainName
    , cbpmRestAPIId

    -- * Destructuring the Response
    , basePathMapping
    , BasePathMapping
    -- * Response Lenses
    , bpmStage
    , bpmBasePath
    , bpmRestAPIId
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests API Gateway to create a new 'BasePathMapping' resource.
--
--
--
-- /See:/ 'createBasePathMapping' smart constructor.
data CreateBasePathMapping = CreateBasePathMapping'
  { _cbpmStage      :: !(Maybe Text)
  , _cbpmBasePath   :: !(Maybe Text)
  , _cbpmDomainName :: !Text
  , _cbpmRestAPIId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBasePathMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbpmStage' - The name of the API's stage that you want to use for this mapping. Leave this blank if you do not want callers to explicitly specify the stage name after any base path name.
--
-- * 'cbpmBasePath' - The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Leave this blank if you do not want callers to specify a base path name after the domain name.
--
-- * 'cbpmDomainName' - [Required] The domain name of the 'BasePathMapping' resource to create.
--
-- * 'cbpmRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
createBasePathMapping
    :: Text -- ^ 'cbpmDomainName'
    -> Text -- ^ 'cbpmRestAPIId'
    -> CreateBasePathMapping
createBasePathMapping pDomainName_ pRestAPIId_ =
  CreateBasePathMapping'
    { _cbpmStage = Nothing
    , _cbpmBasePath = Nothing
    , _cbpmDomainName = pDomainName_
    , _cbpmRestAPIId = pRestAPIId_
    }


-- | The name of the API's stage that you want to use for this mapping. Leave this blank if you do not want callers to explicitly specify the stage name after any base path name.
cbpmStage :: Lens' CreateBasePathMapping (Maybe Text)
cbpmStage = lens _cbpmStage (\ s a -> s{_cbpmStage = a})

-- | The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Leave this blank if you do not want callers to specify a base path name after the domain name.
cbpmBasePath :: Lens' CreateBasePathMapping (Maybe Text)
cbpmBasePath = lens _cbpmBasePath (\ s a -> s{_cbpmBasePath = a})

-- | [Required] The domain name of the 'BasePathMapping' resource to create.
cbpmDomainName :: Lens' CreateBasePathMapping Text
cbpmDomainName = lens _cbpmDomainName (\ s a -> s{_cbpmDomainName = a})

-- | [Required] The string identifier of the associated 'RestApi' .
cbpmRestAPIId :: Lens' CreateBasePathMapping Text
cbpmRestAPIId = lens _cbpmRestAPIId (\ s a -> s{_cbpmRestAPIId = a})

instance AWSRequest CreateBasePathMapping where
        type Rs CreateBasePathMapping = BasePathMapping
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateBasePathMapping where

instance NFData CreateBasePathMapping where

instance ToHeaders CreateBasePathMapping where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateBasePathMapping where
        toJSON CreateBasePathMapping'{..}
          = object
              (catMaybes
                 [("stage" .=) <$> _cbpmStage,
                  ("basePath" .=) <$> _cbpmBasePath,
                  Just ("restApiId" .= _cbpmRestAPIId)])

instance ToPath CreateBasePathMapping where
        toPath CreateBasePathMapping'{..}
          = mconcat
              ["/domainnames/", toBS _cbpmDomainName,
               "/basepathmappings"]

instance ToQuery CreateBasePathMapping where
        toQuery = const mempty
