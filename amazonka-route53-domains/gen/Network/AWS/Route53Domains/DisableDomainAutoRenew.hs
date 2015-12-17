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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation disables automatic renewal of domain registration for the
-- specified domain.
--
-- Caution! Amazon Route 53 doesn\'t have a manual renewal process, so if
-- you disable automatic renewal, registration for the domain will not be
-- renewed when the expiration date passes, and you will lose control of
-- the domain name.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DisableDomainAutoRenew.html AWS API Reference> for DisableDomainAutoRenew.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | /See:/ 'disableDomainAutoRenew' smart constructor.
newtype DisableDomainAutoRenew = DisableDomainAutoRenew'
    { _ddarDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableDomainAutoRenew' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddarDomainName'
disableDomainAutoRenew
    :: Text -- ^ 'ddarDomainName'
    -> DisableDomainAutoRenew
disableDomainAutoRenew pDomainName_ =
    DisableDomainAutoRenew'
    { _ddarDomainName = pDomainName_
    }

-- | Undocumented member.
ddarDomainName :: Lens' DisableDomainAutoRenew Text
ddarDomainName = lens _ddarDomainName (\ s a -> s{_ddarDomainName = a});

instance AWSRequest DisableDomainAutoRenew where
        type Rs DisableDomainAutoRenew =
             DisableDomainAutoRenewResponse
        request = postJSON route53Domains
        response
          = receiveEmpty
              (\ s h x ->
                 DisableDomainAutoRenewResponse' <$>
                   (pure (fromEnum s)))

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableDomainAutoRenewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddarrsResponseStatus'
disableDomainAutoRenewResponse
    :: Int -- ^ 'ddarrsResponseStatus'
    -> DisableDomainAutoRenewResponse
disableDomainAutoRenewResponse pResponseStatus_ =
    DisableDomainAutoRenewResponse'
    { _ddarrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
ddarrsResponseStatus :: Lens' DisableDomainAutoRenewResponse Int
ddarrsResponseStatus = lens _ddarrsResponseStatus (\ s a -> s{_ddarrsResponseStatus = a});
