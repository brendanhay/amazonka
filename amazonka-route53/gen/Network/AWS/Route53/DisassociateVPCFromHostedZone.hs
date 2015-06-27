{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.DisassociateVPCFromHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action disassociates a VPC from an hosted zone.
--
-- To disassociate a VPC to a hosted zone, send a @POST@ request to the
-- @2013-04-01\/hostedzone\/hosted zone ID\/disassociatevpc@ resource. The
-- request body must include an XML document with a
-- @DisassociateVPCFromHostedZoneRequest@ element. The response returns the
-- @DisassociateVPCFromHostedZoneResponse@ element that contains
-- @ChangeInfo@ for you to track the progress of the
-- @DisassociateVPCFromHostedZoneRequest@ you made. See @GetChange@
-- operation for how to track the progress of your change.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DisassociateVPCFromHostedZone.html>
module Network.AWS.Route53.DisassociateVPCFromHostedZone
    (
    -- * Request
      DisassociateVPCFromHostedZone
    -- ** Request constructor
    , disassociateVPCFromHostedZone
    -- ** Request lenses
    , dvfhzComment
    , dvfhzHostedZoneId
    , dvfhzVPC

    -- * Response
    , DisassociateVPCFromHostedZoneResponse
    -- ** Response constructor
    , disassociateVPCFromHostedZoneResponse
    -- ** Response lenses
    , dvfhzrChangeInfo
    , dvfhzrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the request to
-- disassociate a VPC from an hosted zone.
--
-- /See:/ 'disassociateVPCFromHostedZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvfhzComment'
--
-- * 'dvfhzHostedZoneId'
--
-- * 'dvfhzVPC'
data DisassociateVPCFromHostedZone = DisassociateVPCFromHostedZone'
    { _dvfhzComment      :: Maybe Text
    , _dvfhzHostedZoneId :: Text
    , _dvfhzVPC          :: VPC
    } deriving (Eq,Read,Show)

-- | 'DisassociateVPCFromHostedZone' smart constructor.
disassociateVPCFromHostedZone :: Text -> VPC -> DisassociateVPCFromHostedZone
disassociateVPCFromHostedZone pHostedZoneId pVPC =
    DisassociateVPCFromHostedZone'
    { _dvfhzComment = Nothing
    , _dvfhzHostedZoneId = pHostedZoneId
    , _dvfhzVPC = pVPC
    }

-- | /Optional:/ Any comments you want to include about a
-- @DisassociateVPCFromHostedZoneRequest@.
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
        type Sv DisassociateVPCFromHostedZone = Route53
        type Rs DisassociateVPCFromHostedZone =
             DisassociateVPCFromHostedZoneResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 DisassociateVPCFromHostedZoneResponse' <$>
                   (x .@ "ChangeInfo") <*> (pure (fromEnum s)))

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
              ["/2013-04-01/hostedzone/",
               toText _dvfhzHostedZoneId, "/disassociatevpc"]

instance ToQuery DisassociateVPCFromHostedZone where
        toQuery = const mempty

instance ToXML DisassociateVPCFromHostedZone where
        toXML DisassociateVPCFromHostedZone'{..}
          = mconcat
              ["Comment" @= _dvfhzComment, "VPC" @= _dvfhzVPC]

-- | A complex type containing the response information for the request.
--
-- /See:/ 'disassociateVPCFromHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvfhzrChangeInfo'
--
-- * 'dvfhzrStatus'
data DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse'
    { _dvfhzrChangeInfo :: ChangeInfo
    , _dvfhzrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'DisassociateVPCFromHostedZoneResponse' smart constructor.
disassociateVPCFromHostedZoneResponse :: ChangeInfo -> Int -> DisassociateVPCFromHostedZoneResponse
disassociateVPCFromHostedZoneResponse pChangeInfo pStatus =
    DisassociateVPCFromHostedZoneResponse'
    { _dvfhzrChangeInfo = pChangeInfo
    , _dvfhzrStatus = pStatus
    }

-- | A complex type that contains the ID, the status, and the date and time
-- of your @DisassociateVPCFromHostedZoneRequest@.
dvfhzrChangeInfo :: Lens' DisassociateVPCFromHostedZoneResponse ChangeInfo
dvfhzrChangeInfo = lens _dvfhzrChangeInfo (\ s a -> s{_dvfhzrChangeInfo = a});

-- | FIXME: Undocumented member.
dvfhzrStatus :: Lens' DisassociateVPCFromHostedZoneResponse Int
dvfhzrStatus = lens _dvfhzrStatus (\ s a -> s{_dvfhzrStatus = a});
