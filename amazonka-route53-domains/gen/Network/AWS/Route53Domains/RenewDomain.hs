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
-- Module      : Network.AWS.Route53Domains.RenewDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.
--
--
-- We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html Renewing Registration for a Domain> in the Amazon Route 53 Developer Guide.
--
module Network.AWS.Route53Domains.RenewDomain
    (
    -- * Creating a Request
      renewDomain
    , RenewDomain
    -- * Request Lenses
    , rdDurationInYears
    , rdDomainName
    , rdCurrentExpiryYear

    -- * Destructuring the Response
    , renewDomainResponse
    , RenewDomainResponse
    -- * Response Lenses
    , rrsResponseStatus
    , rrsOperationId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | A @RenewDomain@ request includes the number of years that you want to renew for and the current expiration year.
--
--
--
-- /See:/ 'renewDomain' smart constructor.
data RenewDomain = RenewDomain'
  { _rdDurationInYears   :: !(Maybe Nat)
  , _rdDomainName        :: !Text
  , _rdCurrentExpiryYear :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdDurationInYears' - The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . Default: 1
--
-- * 'rdDomainName' - The name of the domain that you want to renew.
--
-- * 'rdCurrentExpiryYear' - The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
renewDomain
    :: Text -- ^ 'rdDomainName'
    -> Int -- ^ 'rdCurrentExpiryYear'
    -> RenewDomain
renewDomain pDomainName_ pCurrentExpiryYear_ =
  RenewDomain'
    { _rdDurationInYears = Nothing
    , _rdDomainName = pDomainName_
    , _rdCurrentExpiryYear = pCurrentExpiryYear_
    }


-- | The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . Default: 1
rdDurationInYears :: Lens' RenewDomain (Maybe Natural)
rdDurationInYears = lens _rdDurationInYears (\ s a -> s{_rdDurationInYears = a}) . mapping _Nat

-- | The name of the domain that you want to renew.
rdDomainName :: Lens' RenewDomain Text
rdDomainName = lens _rdDomainName (\ s a -> s{_rdDomainName = a})

-- | The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
rdCurrentExpiryYear :: Lens' RenewDomain Int
rdCurrentExpiryYear = lens _rdCurrentExpiryYear (\ s a -> s{_rdCurrentExpiryYear = a})

instance AWSRequest RenewDomain where
        type Rs RenewDomain = RenewDomainResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 RenewDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable RenewDomain where

instance NFData RenewDomain where

instance ToHeaders RenewDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.RenewDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RenewDomain where
        toJSON RenewDomain'{..}
          = object
              (catMaybes
                 [("DurationInYears" .=) <$> _rdDurationInYears,
                  Just ("DomainName" .= _rdDomainName),
                  Just ("CurrentExpiryYear" .= _rdCurrentExpiryYear)])

instance ToPath RenewDomain where
        toPath = const "/"

instance ToQuery RenewDomain where
        toQuery = const mempty

-- | /See:/ 'renewDomainResponse' smart constructor.
data RenewDomainResponse = RenewDomainResponse'
  { _rrsResponseStatus :: !Int
  , _rrsOperationId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsResponseStatus' - -- | The response status code.
--
-- * 'rrsOperationId' - The identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
renewDomainResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> Text -- ^ 'rrsOperationId'
    -> RenewDomainResponse
renewDomainResponse pResponseStatus_ pOperationId_ =
  RenewDomainResponse'
    {_rrsResponseStatus = pResponseStatus_, _rrsOperationId = pOperationId_}


-- | -- | The response status code.
rrsResponseStatus :: Lens' RenewDomainResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

-- | The identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
rrsOperationId :: Lens' RenewDomainResponse Text
rrsOperationId = lens _rrsOperationId (\ s a -> s{_rrsOperationId = a})

instance NFData RenewDomainResponse where
