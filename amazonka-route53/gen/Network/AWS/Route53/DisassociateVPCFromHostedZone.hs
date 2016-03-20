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
-- Module      : Network.AWS.Route53.DisassociateVPCFromHostedZone
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action disassociates a VPC from an hosted zone.
--
-- To disassociate a VPC to a hosted zone, send a 'POST' request to the
-- '\/Route 53 API version\/hostedzone\/hosted zone ID\/disassociatevpc'
-- resource. The request body must include a document with a
-- 'DisassociateVPCFromHostedZoneRequest' element. The response returns the
-- 'DisassociateVPCFromHostedZoneResponse' element that contains
-- 'ChangeInfo' for you to track the progress of the
-- 'DisassociateVPCFromHostedZoneRequest' you made. See 'GetChange'
-- operation for how to track the progress of your change.
module Network.AWS.Route53.DisassociateVPCFromHostedZone
    (
    -- * Creating a Request
      disassociateVPCFromHostedZone
    , DisassociateVPCFromHostedZone
    -- * Request Lenses
    , dvfhzComment
    , dvfhzHostedZoneId
    , dvfhzVPC

    -- * Destructuring the Response
    , disassociateVPCFromHostedZoneResponse
    , DisassociateVPCFromHostedZoneResponse
    -- * Response Lenses
    , dvfhzrsResponseStatus
    , dvfhzrsChangeInfo
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to
-- disassociate a VPC from an hosted zone.
--
-- /See:/ 'disassociateVPCFromHostedZone' smart constructor.
data DisassociateVPCFromHostedZone = DisassociateVPCFromHostedZone'
    { _dvfhzComment      :: !(Maybe Text)
    , _dvfhzHostedZoneId :: !Text
    , _dvfhzVPC          :: !VPC
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisassociateVPCFromHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvfhzComment'
--
-- * 'dvfhzHostedZoneId'
--
-- * 'dvfhzVPC'
disassociateVPCFromHostedZone
    :: Text -- ^ 'dvfhzHostedZoneId'
    -> VPC -- ^ 'dvfhzVPC'
    -> DisassociateVPCFromHostedZone
disassociateVPCFromHostedZone pHostedZoneId_ pVPC_ =
    DisassociateVPCFromHostedZone'
    { _dvfhzComment = Nothing
    , _dvfhzHostedZoneId = pHostedZoneId_
    , _dvfhzVPC = pVPC_
    }

-- | /Optional:/ Any comments you want to include about a
-- 'DisassociateVPCFromHostedZoneRequest'.
dvfhzComment :: Lens' DisassociateVPCFromHostedZone (Maybe Text)
dvfhzComment = lens _dvfhzComment (\ s a -> s{_dvfhzComment = a});

-- | The ID of the hosted zone you want to disassociate your VPC from.
--
-- Note that you cannot disassociate the last VPC from a hosted zone.
dvfhzHostedZoneId :: Lens' DisassociateVPCFromHostedZone Text
dvfhzHostedZoneId = lens _dvfhzHostedZoneId (\ s a -> s{_dvfhzHostedZoneId = a});

-- | The VPC that you want your hosted zone to be disassociated from.
dvfhzVPC :: Lens' DisassociateVPCFromHostedZone VPC
dvfhzVPC = lens _dvfhzVPC (\ s a -> s{_dvfhzVPC = a});

instance AWSRequest DisassociateVPCFromHostedZone
         where
        type Rs DisassociateVPCFromHostedZone =
             DisassociateVPCFromHostedZoneResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 DisassociateVPCFromHostedZoneResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance Hashable DisassociateVPCFromHostedZone

instance ToElement DisassociateVPCFromHostedZone
         where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}DisassociateVPCFromHostedZoneRequest"

instance ToHeaders DisassociateVPCFromHostedZone
         where
        toHeaders = const mempty

instance ToPath DisassociateVPCFromHostedZone where
        toPath DisassociateVPCFromHostedZone'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toBS _dvfhzHostedZoneId,
               "/disassociatevpc"]

instance ToQuery DisassociateVPCFromHostedZone where
        toQuery = const mempty

instance ToXML DisassociateVPCFromHostedZone where
        toXML DisassociateVPCFromHostedZone'{..}
          = mconcat
              ["Comment" @= _dvfhzComment, "VPC" @= _dvfhzVPC]

-- | A complex type containing the response information for the request.
--
-- /See:/ 'disassociateVPCFromHostedZoneResponse' smart constructor.
data DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse'
    { _dvfhzrsResponseStatus :: !Int
    , _dvfhzrsChangeInfo     :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisassociateVPCFromHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvfhzrsResponseStatus'
--
-- * 'dvfhzrsChangeInfo'
disassociateVPCFromHostedZoneResponse
    :: Int -- ^ 'dvfhzrsResponseStatus'
    -> ChangeInfo -- ^ 'dvfhzrsChangeInfo'
    -> DisassociateVPCFromHostedZoneResponse
disassociateVPCFromHostedZoneResponse pResponseStatus_ pChangeInfo_ =
    DisassociateVPCFromHostedZoneResponse'
    { _dvfhzrsResponseStatus = pResponseStatus_
    , _dvfhzrsChangeInfo = pChangeInfo_
    }

-- | The response status code.
dvfhzrsResponseStatus :: Lens' DisassociateVPCFromHostedZoneResponse Int
dvfhzrsResponseStatus = lens _dvfhzrsResponseStatus (\ s a -> s{_dvfhzrsResponseStatus = a});

-- | A complex type that contains the ID, the status, and the date and time
-- of your 'DisassociateVPCFromHostedZoneRequest'.
dvfhzrsChangeInfo :: Lens' DisassociateVPCFromHostedZoneResponse ChangeInfo
dvfhzrsChangeInfo = lens _dvfhzrsChangeInfo (\ s a -> s{_dvfhzrsChangeInfo = a});
