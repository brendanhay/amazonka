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
-- Module      : Network.AWS.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures Amazon Route 53 to automatically renew the specified domain before the domain registration expires. The cost of renewing your domain registration is billed to your AWS account.
--
--
-- The period during which you can renew a domain name varies by TLD. For a list of TLDs and their renewal policies, see <http://wiki.gandi.net/en/domains/renew#renewal_restoration_and_deletion_times "Renewal, restoration, and deletion times"> on the website for our registrar associate, Gandi. Amazon Route 53 requires that you renew before the end of the renewal period that is listed on the Gandi website so we can complete processing before the deadline.
--
module Network.AWS.Route53Domains.EnableDomainAutoRenew
    (
    -- * Creating a Request
      enableDomainAutoRenew
    , EnableDomainAutoRenew
    -- * Request Lenses
    , edarDomainName

    -- * Destructuring the Response
    , enableDomainAutoRenewResponse
    , EnableDomainAutoRenewResponse
    -- * Response Lenses
    , edarrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | /See:/ 'enableDomainAutoRenew' smart constructor.
newtype EnableDomainAutoRenew = EnableDomainAutoRenew'
  { _edarDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableDomainAutoRenew' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edarDomainName' - The name of the domain that you want to enable automatic renewal for.
enableDomainAutoRenew
    :: Text -- ^ 'edarDomainName'
    -> EnableDomainAutoRenew
enableDomainAutoRenew pDomainName_ =
  EnableDomainAutoRenew' {_edarDomainName = pDomainName_}


-- | The name of the domain that you want to enable automatic renewal for.
edarDomainName :: Lens' EnableDomainAutoRenew Text
edarDomainName = lens _edarDomainName (\ s a -> s{_edarDomainName = a})

instance AWSRequest EnableDomainAutoRenew where
        type Rs EnableDomainAutoRenew =
             EnableDomainAutoRenewResponse
        request = postJSON route53Domains
        response
          = receiveEmpty
              (\ s h x ->
                 EnableDomainAutoRenewResponse' <$>
                   (pure (fromEnum s)))

instance Hashable EnableDomainAutoRenew where

instance NFData EnableDomainAutoRenew where

instance ToHeaders EnableDomainAutoRenew where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.EnableDomainAutoRenew" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableDomainAutoRenew where
        toJSON EnableDomainAutoRenew'{..}
          = object
              (catMaybes [Just ("DomainName" .= _edarDomainName)])

instance ToPath EnableDomainAutoRenew where
        toPath = const "/"

instance ToQuery EnableDomainAutoRenew where
        toQuery = const mempty

-- | /See:/ 'enableDomainAutoRenewResponse' smart constructor.
newtype EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse'
  { _edarrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableDomainAutoRenewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edarrsResponseStatus' - -- | The response status code.
enableDomainAutoRenewResponse
    :: Int -- ^ 'edarrsResponseStatus'
    -> EnableDomainAutoRenewResponse
enableDomainAutoRenewResponse pResponseStatus_ =
  EnableDomainAutoRenewResponse' {_edarrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
edarrsResponseStatus :: Lens' EnableDomainAutoRenewResponse Int
edarrsResponseStatus = lens _edarrsResponseStatus (\ s a -> s{_edarrsResponseStatus = a})

instance NFData EnableDomainAutoRenewResponse where
