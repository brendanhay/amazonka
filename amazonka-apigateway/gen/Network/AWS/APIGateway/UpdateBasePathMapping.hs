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
-- Module      : Network.AWS.APIGateway.UpdateBasePathMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the 'BasePathMapping' resource.
--
--
module Network.AWS.APIGateway.UpdateBasePathMapping
    (
    -- * Creating a Request
      updateBasePathMapping
    , UpdateBasePathMapping
    -- * Request Lenses
    , ubpmPatchOperations
    , ubpmDomainName
    , ubpmBasePath

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

-- | A request to change information about the 'BasePathMapping' resource.
--
--
--
-- /See:/ 'updateBasePathMapping' smart constructor.
data UpdateBasePathMapping = UpdateBasePathMapping'
  { _ubpmPatchOperations :: !(Maybe [PatchOperation])
  , _ubpmDomainName      :: !Text
  , _ubpmBasePath        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBasePathMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubpmPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'ubpmDomainName' - [Required] The domain name of the 'BasePathMapping' resource to change.
--
-- * 'ubpmBasePath' - [Required] The base path of the 'BasePathMapping' resource to change.
updateBasePathMapping
    :: Text -- ^ 'ubpmDomainName'
    -> Text -- ^ 'ubpmBasePath'
    -> UpdateBasePathMapping
updateBasePathMapping pDomainName_ pBasePath_ =
  UpdateBasePathMapping'
    { _ubpmPatchOperations = Nothing
    , _ubpmDomainName = pDomainName_
    , _ubpmBasePath = pBasePath_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
ubpmPatchOperations :: Lens' UpdateBasePathMapping [PatchOperation]
ubpmPatchOperations = lens _ubpmPatchOperations (\ s a -> s{_ubpmPatchOperations = a}) . _Default . _Coerce

-- | [Required] The domain name of the 'BasePathMapping' resource to change.
ubpmDomainName :: Lens' UpdateBasePathMapping Text
ubpmDomainName = lens _ubpmDomainName (\ s a -> s{_ubpmDomainName = a})

-- | [Required] The base path of the 'BasePathMapping' resource to change.
ubpmBasePath :: Lens' UpdateBasePathMapping Text
ubpmBasePath = lens _ubpmBasePath (\ s a -> s{_ubpmBasePath = a})

instance AWSRequest UpdateBasePathMapping where
        type Rs UpdateBasePathMapping = BasePathMapping
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateBasePathMapping where

instance NFData UpdateBasePathMapping where

instance ToHeaders UpdateBasePathMapping where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateBasePathMapping where
        toJSON UpdateBasePathMapping'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _ubpmPatchOperations])

instance ToPath UpdateBasePathMapping where
        toPath UpdateBasePathMapping'{..}
          = mconcat
              ["/domainnames/", toBS _ubpmDomainName,
               "/basepathmappings/", toBS _ubpmBasePath]

instance ToQuery UpdateBasePathMapping where
        toQuery = const mempty
