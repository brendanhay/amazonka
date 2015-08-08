{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures Amazon Route 53 to automatically renew the
-- specified domain before the domain registration expires. The cost of
-- renewing your domain registration is billed to your AWS account.
--
-- The period during which you can renew a domain name varies by TLD. For a
-- list of TLDs and their renewal policies, see
-- <http://wiki.gandi.net/en/domains/renew#renewal_restoration_and_deletion_times \"Renewal, restoration, and deletion times\">
-- on the website for our registrar partner, Gandi. Route 53 requires that
-- you renew before the end of the renewal period that is listed on the
-- Gandi website so we can complete processing before the deadline.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-EnableDomainAutoRenew.html AWS API Reference> for EnableDomainAutoRenew.
module Network.AWS.Route53Domains.EnableDomainAutoRenew
    (
    -- * Creating a Request
      EnableDomainAutoRenew
    , enableDomainAutoRenew
    -- * Request Lenses
    , edarDomainName

    -- * Destructuring the Response
    , EnableDomainAutoRenewResponse
    , enableDomainAutoRenewResponse
    -- * Response Lenses
    , edarrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | /See:/ 'enableDomainAutoRenew' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edarDomainName'
newtype EnableDomainAutoRenew = EnableDomainAutoRenew'
    { _edarDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableDomainAutoRenew' smart constructor.
enableDomainAutoRenew :: Text -> EnableDomainAutoRenew
enableDomainAutoRenew pDomainName_ =
    EnableDomainAutoRenew'
    { _edarDomainName = pDomainName_
    }

-- | Undocumented member.
edarDomainName :: Lens' EnableDomainAutoRenew Text
edarDomainName = lens _edarDomainName (\ s a -> s{_edarDomainName = a});

instance AWSRequest EnableDomainAutoRenew where
        type Sv EnableDomainAutoRenew = Route53Domains
        type Rs EnableDomainAutoRenew =
             EnableDomainAutoRenewResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 EnableDomainAutoRenewResponse' <$>
                   (pure (fromEnum s)))

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
          = object ["DomainName" .= _edarDomainName]

instance ToPath EnableDomainAutoRenew where
        toPath = const "/"

instance ToQuery EnableDomainAutoRenew where
        toQuery = const mempty

-- | /See:/ 'enableDomainAutoRenewResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edarrsStatus'
newtype EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse'
    { _edarrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableDomainAutoRenewResponse' smart constructor.
enableDomainAutoRenewResponse :: Int -> EnableDomainAutoRenewResponse
enableDomainAutoRenewResponse pStatus_ =
    EnableDomainAutoRenewResponse'
    { _edarrsStatus = pStatus_
    }

-- | Undocumented member.
edarrsStatus :: Lens' EnableDomainAutoRenewResponse Int
edarrsStatus = lens _edarrsStatus (\ s a -> s{_edarrsStatus = a});
