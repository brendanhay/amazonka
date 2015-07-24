{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainNameservers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation replaces the current set of name servers for the domain
-- with the specified set of name servers. If you use Amazon Route 53 as
-- your DNS service, specify the four name servers in the delegation set
-- for the hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use
-- to track the progress and completion of the action. If the request is
-- not completed successfully, the domain registrant will be notified by
-- email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainNameservers.html>
module Network.AWS.Route53Domains.UpdateDomainNameservers
    (
    -- * Request
      UpdateDomainNameservers
    -- ** Request constructor
    , updateDomainNameservers
    -- ** Request lenses
    , udnFIAuthKey
    , udnDomainName
    , udnNameservers

    -- * Response
    , UpdateDomainNameserversResponse
    -- ** Response constructor
    , updateDomainNameserversResponse
    -- ** Response lenses
    , udnrsStatus
    , udnrsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The UpdateDomainNameserver request includes the following elements.
--
-- /See:/ 'updateDomainNameservers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udnFIAuthKey'
--
-- * 'udnDomainName'
--
-- * 'udnNameservers'
data UpdateDomainNameservers = UpdateDomainNameservers'
    { _udnFIAuthKey   :: !(Maybe Text)
    , _udnDomainName  :: !Text
    , _udnNameservers :: ![Nameserver]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainNameservers' smart constructor.
updateDomainNameservers :: Text -> UpdateDomainNameservers
updateDomainNameservers pDomainName_ =
    UpdateDomainNameservers'
    { _udnFIAuthKey = Nothing
    , _udnDomainName = pDomainName_
    , _udnNameservers = mempty
    }

-- | The authorization key for .fi domains
udnFIAuthKey :: Lens' UpdateDomainNameservers (Maybe Text)
udnFIAuthKey = lens _udnFIAuthKey (\ s a -> s{_udnFIAuthKey = a});

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
udnDomainName :: Lens' UpdateDomainNameservers Text
udnDomainName = lens _udnDomainName (\ s a -> s{_udnDomainName = a});

-- | A list of new name servers for the domain.
--
-- Type: Complex
--
-- Children: @Name@, @GlueIps@
--
-- Required: Yes
udnNameservers :: Lens' UpdateDomainNameservers [Nameserver]
udnNameservers = lens _udnNameservers (\ s a -> s{_udnNameservers = a});

instance AWSRequest UpdateDomainNameservers where
        type Sv UpdateDomainNameservers = Route53Domains
        type Rs UpdateDomainNameservers =
             UpdateDomainNameserversResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainNameserversResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance ToHeaders UpdateDomainNameservers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.UpdateDomainNameservers"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDomainNameservers where
        toJSON UpdateDomainNameservers'{..}
          = object
              ["FIAuthKey" .= _udnFIAuthKey,
               "DomainName" .= _udnDomainName,
               "Nameservers" .= _udnNameservers]

instance ToPath UpdateDomainNameservers where
        toPath = const "/"

instance ToQuery UpdateDomainNameservers where
        toQuery = const mempty

-- | The UpdateDomainNameservers response includes the following element.
--
-- /See:/ 'updateDomainNameserversResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udnrsStatus'
--
-- * 'udnrsOperationId'
data UpdateDomainNameserversResponse = UpdateDomainNameserversResponse'
    { _udnrsStatus      :: !Int
    , _udnrsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainNameserversResponse' smart constructor.
updateDomainNameserversResponse :: Int -> Text -> UpdateDomainNameserversResponse
updateDomainNameserversResponse pStatus_ pOperationId_ =
    UpdateDomainNameserversResponse'
    { _udnrsStatus = pStatus_
    , _udnrsOperationId = pOperationId_
    }

-- | FIXME: Undocumented member.
udnrsStatus :: Lens' UpdateDomainNameserversResponse Int
udnrsStatus = lens _udnrsStatus (\ s a -> s{_udnrsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
udnrsOperationId :: Lens' UpdateDomainNameserversResponse Text
udnrsOperationId = lens _udnrsOperationId (\ s a -> s{_udnrsOperationId = a});
