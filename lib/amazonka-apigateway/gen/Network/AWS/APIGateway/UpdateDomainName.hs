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
-- Module      : Network.AWS.APIGateway.UpdateDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the 'DomainName' resource.
--
--
module Network.AWS.APIGateway.UpdateDomainName
    (
    -- * Creating a Request
      updateDomainName
    , UpdateDomainName
    -- * Request Lenses
    , udnPatchOperations
    , udnDomainName

    -- * Destructuring the Response
    , domainName
    , DomainName
    -- * Response Lenses
    , dnRegionalHostedZoneId
    , dnCertificateName
    , dnRegionalCertificateARN
    , dnCertificateARN
    , dnDistributionHostedZoneId
    , dnDomainName
    , dnRegionalCertificateName
    , dnRegionalDomainName
    , dnCertificateUploadDate
    , dnDistributionDomainName
    , dnEndpointConfiguration
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to change information about the 'DomainName' resource.
--
--
--
-- /See:/ 'updateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { _udnPatchOperations :: !(Maybe [PatchOperation])
  , _udnDomainName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udnPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'udnDomainName' - [Required] The name of the 'DomainName' resource to be changed.
updateDomainName
    :: Text -- ^ 'udnDomainName'
    -> UpdateDomainName
updateDomainName pDomainName_ =
  UpdateDomainName'
    {_udnPatchOperations = Nothing, _udnDomainName = pDomainName_}


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
udnPatchOperations :: Lens' UpdateDomainName [PatchOperation]
udnPatchOperations = lens _udnPatchOperations (\ s a -> s{_udnPatchOperations = a}) . _Default . _Coerce

-- | [Required] The name of the 'DomainName' resource to be changed.
udnDomainName :: Lens' UpdateDomainName Text
udnDomainName = lens _udnDomainName (\ s a -> s{_udnDomainName = a})

instance AWSRequest UpdateDomainName where
        type Rs UpdateDomainName = DomainName
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateDomainName where

instance NFData UpdateDomainName where

instance ToHeaders UpdateDomainName where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateDomainName where
        toJSON UpdateDomainName'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _udnPatchOperations])

instance ToPath UpdateDomainName where
        toPath UpdateDomainName'{..}
          = mconcat ["/domainnames/", toBS _udnDomainName]

instance ToQuery UpdateDomainName where
        toQuery = const mempty
