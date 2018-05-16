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
-- Module      : Network.AWS.APIGateway.DeleteBasePathMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'BasePathMapping' resource.
--
--
module Network.AWS.APIGateway.DeleteBasePathMapping
    (
    -- * Creating a Request
      deleteBasePathMapping
    , DeleteBasePathMapping
    -- * Request Lenses
    , dbpmDomainName
    , dbpmBasePath

    -- * Destructuring the Response
    , deleteBasePathMappingResponse
    , DeleteBasePathMappingResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to delete the 'BasePathMapping' resource.
--
--
--
-- /See:/ 'deleteBasePathMapping' smart constructor.
data DeleteBasePathMapping = DeleteBasePathMapping'
  { _dbpmDomainName :: !Text
  , _dbpmBasePath   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBasePathMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpmDomainName' - [Required] The domain name of the 'BasePathMapping' resource to delete.
--
-- * 'dbpmBasePath' - [Required] The base path name of the 'BasePathMapping' resource to delete.
deleteBasePathMapping
    :: Text -- ^ 'dbpmDomainName'
    -> Text -- ^ 'dbpmBasePath'
    -> DeleteBasePathMapping
deleteBasePathMapping pDomainName_ pBasePath_ =
  DeleteBasePathMapping'
    {_dbpmDomainName = pDomainName_, _dbpmBasePath = pBasePath_}


-- | [Required] The domain name of the 'BasePathMapping' resource to delete.
dbpmDomainName :: Lens' DeleteBasePathMapping Text
dbpmDomainName = lens _dbpmDomainName (\ s a -> s{_dbpmDomainName = a})

-- | [Required] The base path name of the 'BasePathMapping' resource to delete.
dbpmBasePath :: Lens' DeleteBasePathMapping Text
dbpmBasePath = lens _dbpmBasePath (\ s a -> s{_dbpmBasePath = a})

instance AWSRequest DeleteBasePathMapping where
        type Rs DeleteBasePathMapping =
             DeleteBasePathMappingResponse
        request = delete apiGateway
        response = receiveNull DeleteBasePathMappingResponse'

instance Hashable DeleteBasePathMapping where

instance NFData DeleteBasePathMapping where

instance ToHeaders DeleteBasePathMapping where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteBasePathMapping where
        toPath DeleteBasePathMapping'{..}
          = mconcat
              ["/domainnames/", toBS _dbpmDomainName,
               "/basepathmappings/", toBS _dbpmBasePath]

instance ToQuery DeleteBasePathMapping where
        toQuery = const mempty

-- | /See:/ 'deleteBasePathMappingResponse' smart constructor.
data DeleteBasePathMappingResponse =
  DeleteBasePathMappingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBasePathMappingResponse' with the minimum fields required to make a request.
--
deleteBasePathMappingResponse
    :: DeleteBasePathMappingResponse
deleteBasePathMappingResponse = DeleteBasePathMappingResponse'


instance NFData DeleteBasePathMappingResponse where
