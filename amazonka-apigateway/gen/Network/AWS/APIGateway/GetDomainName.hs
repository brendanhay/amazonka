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
-- Module      : Network.AWS.APIGateway.GetDomainName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a domain name that is contained in a simpler, more intuitive URL that can be called.
--
--
module Network.AWS.APIGateway.GetDomainName
    (
    -- * Creating a Request
      getDomainName
    , GetDomainName
    -- * Request Lenses
    , gdnDomainName

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

-- | Request to get the name of a 'DomainName' resource.
--
--
--
-- /See:/ 'getDomainName' smart constructor.
newtype GetDomainName = GetDomainName'
  { _gdnDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDomainName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdnDomainName' - [Required] The name of the 'DomainName' resource.
getDomainName
    :: Text -- ^ 'gdnDomainName'
    -> GetDomainName
getDomainName pDomainName_ = GetDomainName' {_gdnDomainName = pDomainName_}


-- | [Required] The name of the 'DomainName' resource.
gdnDomainName :: Lens' GetDomainName Text
gdnDomainName = lens _gdnDomainName (\ s a -> s{_gdnDomainName = a})

instance AWSRequest GetDomainName where
        type Rs GetDomainName = DomainName
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetDomainName where

instance NFData GetDomainName where

instance ToHeaders GetDomainName where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetDomainName where
        toPath GetDomainName'{..}
          = mconcat ["/domainnames/", toBS _gdnDomainName]

instance ToQuery GetDomainName where
        toQuery = const mempty
