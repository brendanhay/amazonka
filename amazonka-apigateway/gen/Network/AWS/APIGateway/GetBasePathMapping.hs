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
-- Module      : Network.AWS.APIGateway.GetBasePathMapping
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a BasePathMapping resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/GetBasePathMapping.html AWS API Reference> for GetBasePathMapping.
module Network.AWS.APIGateway.GetBasePathMapping
    (
    -- * Creating a Request
      getBasePathMapping
    , GetBasePathMapping
    -- * Request Lenses
    , gbpmDomainName
    , gbpmBasePath

    -- * Destructuring the Response
    , basePathMapping
    , BasePathMapping
    -- * Response Lenses
    , bpmStage
    , bpmBasePath
    , bpmRestAPIId
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to describe a BasePathMapping resource.
--
-- /See:/ 'getBasePathMapping' smart constructor.
data GetBasePathMapping = GetBasePathMapping'
    { _gbpmDomainName :: !Text
    , _gbpmBasePath   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBasePathMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpmDomainName'
--
-- * 'gbpmBasePath'
getBasePathMapping
    :: Text -- ^ 'gbpmDomainName'
    -> Text -- ^ 'gbpmBasePath'
    -> GetBasePathMapping
getBasePathMapping pDomainName_ pBasePath_ =
    GetBasePathMapping'
    { _gbpmDomainName = pDomainName_
    , _gbpmBasePath = pBasePath_
    }

-- | The domain name of the BasePathMapping resource to be described.
gbpmDomainName :: Lens' GetBasePathMapping Text
gbpmDomainName = lens _gbpmDomainName (\ s a -> s{_gbpmDomainName = a});

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name. This value must be unique for all of the
-- mappings across a single API. Leave this blank if you do not want
-- callers to specify any base path name after the domain name.
gbpmBasePath :: Lens' GetBasePathMapping Text
gbpmBasePath = lens _gbpmBasePath (\ s a -> s{_gbpmBasePath = a});

instance AWSRequest GetBasePathMapping where
        type Rs GetBasePathMapping = BasePathMapping
        request = get aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders GetBasePathMapping where
        toHeaders = const mempty

instance ToPath GetBasePathMapping where
        toPath GetBasePathMapping'{..}
          = mconcat
              ["/domainnames/", toBS _gbpmDomainName,
               "/basepathmappings/", toBS _gbpmBasePath]

instance ToQuery GetBasePathMapping where
        toQuery = const mempty
