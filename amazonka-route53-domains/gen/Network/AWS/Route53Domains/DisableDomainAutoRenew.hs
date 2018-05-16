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
-- Module      : Network.AWS.Route53Domains.DisableDomainAutoRenew
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation disables automatic renewal of domain registration for the specified domain.
--
--
module Network.AWS.Route53Domains.DisableDomainAutoRenew
    (
    -- * Creating a Request
      disableDomainAutoRenew
    , DisableDomainAutoRenew
    -- * Request Lenses
    , ddarDomainName

    -- * Destructuring the Response
    , disableDomainAutoRenewResponse
    , DisableDomainAutoRenewResponse
    -- * Response Lenses
    , ddarrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | /See:/ 'disableDomainAutoRenew' smart constructor.
newtype DisableDomainAutoRenew = DisableDomainAutoRenew'
  { _ddarDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableDomainAutoRenew' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddarDomainName' - The name of the domain that you want to disable automatic renewal for.
disableDomainAutoRenew
    :: Text -- ^ 'ddarDomainName'
    -> DisableDomainAutoRenew
disableDomainAutoRenew pDomainName_ =
  DisableDomainAutoRenew' {_ddarDomainName = pDomainName_}


-- | The name of the domain that you want to disable automatic renewal for.
ddarDomainName :: Lens' DisableDomainAutoRenew Text
ddarDomainName = lens _ddarDomainName (\ s a -> s{_ddarDomainName = a})

instance AWSRequest DisableDomainAutoRenew where
        type Rs DisableDomainAutoRenew =
             DisableDomainAutoRenewResponse
        request = postJSON route53Domains
        response
          = receiveEmpty
              (\ s h x ->
                 DisableDomainAutoRenewResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisableDomainAutoRenew where

instance NFData DisableDomainAutoRenew where

instance ToHeaders DisableDomainAutoRenew where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.DisableDomainAutoRenew" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableDomainAutoRenew where
        toJSON DisableDomainAutoRenew'{..}
          = object
              (catMaybes [Just ("DomainName" .= _ddarDomainName)])

instance ToPath DisableDomainAutoRenew where
        toPath = const "/"

instance ToQuery DisableDomainAutoRenew where
        toQuery = const mempty

-- | /See:/ 'disableDomainAutoRenewResponse' smart constructor.
newtype DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse'
  { _ddarrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableDomainAutoRenewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddarrsResponseStatus' - -- | The response status code.
disableDomainAutoRenewResponse
    :: Int -- ^ 'ddarrsResponseStatus'
    -> DisableDomainAutoRenewResponse
disableDomainAutoRenewResponse pResponseStatus_ =
  DisableDomainAutoRenewResponse' {_ddarrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddarrsResponseStatus :: Lens' DisableDomainAutoRenewResponse Int
ddarrsResponseStatus = lens _ddarrsResponseStatus (\ s a -> s{_ddarrsResponseStatus = a})

instance NFData DisableDomainAutoRenewResponse where
