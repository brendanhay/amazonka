{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action associates a VPC with an hosted zone.
--
-- To associate a VPC with an hosted zone, send a @POST@ request to the
-- @2013-04-01\/hostedzone\/hosted zone ID\/associatevpc@ resource. The
-- request body must include an XML document with a
-- @AssociateVPCWithHostedZoneRequest@ element. The response returns the
-- @AssociateVPCWithHostedZoneResponse@ element that contains @ChangeInfo@
-- for you to track the progress of the @AssociateVPCWithHostedZoneRequest@
-- you made. See @GetChange@ operation for how to track the progress of
-- your change.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html>
module Network.AWS.Route53.AssociateVPCWithHostedZone
    (
    -- * Request
      AssociateVPCWithHostedZone
    -- ** Request constructor
    , associateVPCWithHostedZone
    -- ** Request lenses
    , avwhzrqComment
    , avwhzrqHostedZoneId
    , avwhzrqVPC

    -- * Response
    , AssociateVPCWithHostedZoneResponse
    -- ** Response constructor
    , associateVPCWithHostedZoneResponse
    -- ** Response lenses
    , avwhzrsStatus
    , avwhzrsChangeInfo
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the request to associate
-- a VPC with an hosted zone.
--
-- /See:/ 'associateVPCWithHostedZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avwhzrqComment'
--
-- * 'avwhzrqHostedZoneId'
--
-- * 'avwhzrqVPC'
data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone'
    { _avwhzrqComment      :: !(Maybe Text)
    , _avwhzrqHostedZoneId :: !Text
    , _avwhzrqVPC          :: !VPC
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateVPCWithHostedZone' smart constructor.
associateVPCWithHostedZone :: Text -> VPC -> AssociateVPCWithHostedZone
associateVPCWithHostedZone pHostedZoneId_ pVPC_ =
    AssociateVPCWithHostedZone'
    { _avwhzrqComment = Nothing
    , _avwhzrqHostedZoneId = pHostedZoneId_
    , _avwhzrqVPC = pVPC_
    }

-- | /Optional:/ Any comments you want to include about a
-- @AssociateVPCWithHostedZoneRequest@.
avwhzrqComment :: Lens' AssociateVPCWithHostedZone (Maybe Text)
avwhzrqComment = lens _avwhzrqComment (\ s a -> s{_avwhzrqComment = a});

-- | The ID of the hosted zone you want to associate your VPC with.
--
-- Note that you cannot associate a VPC with a hosted zone that doesn\'t
-- have an existing VPC association.
avwhzrqHostedZoneId :: Lens' AssociateVPCWithHostedZone Text
avwhzrqHostedZoneId = lens _avwhzrqHostedZoneId (\ s a -> s{_avwhzrqHostedZoneId = a});

-- | The VPC that you want your hosted zone to be associated with.
avwhzrqVPC :: Lens' AssociateVPCWithHostedZone VPC
avwhzrqVPC = lens _avwhzrqVPC (\ s a -> s{_avwhzrqVPC = a});

instance AWSRequest AssociateVPCWithHostedZone where
        type Sv AssociateVPCWithHostedZone = Route53
        type Rs AssociateVPCWithHostedZone =
             AssociateVPCWithHostedZoneResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 AssociateVPCWithHostedZoneResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance ToElement AssociateVPCWithHostedZone where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}AssociateVPCWithHostedZoneRequest"

instance ToHeaders AssociateVPCWithHostedZone where
        toHeaders = const mempty

instance ToPath AssociateVPCWithHostedZone where
        toPath AssociateVPCWithHostedZone'{..}
          = mconcat
              ["/2013-04-01/hostedzone/",
               toText _avwhzrqHostedZoneId, "/associatevpc"]

instance ToQuery AssociateVPCWithHostedZone where
        toQuery = const mempty

instance ToXML AssociateVPCWithHostedZone where
        toXML AssociateVPCWithHostedZone'{..}
          = mconcat
              ["Comment" @= _avwhzrqComment, "VPC" @= _avwhzrqVPC]

-- | A complex type containing the response information for the request.
--
-- /See:/ 'associateVPCWithHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avwhzrsStatus'
--
-- * 'avwhzrsChangeInfo'
data AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse'
    { _avwhzrsStatus     :: !Int
    , _avwhzrsChangeInfo :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateVPCWithHostedZoneResponse' smart constructor.
associateVPCWithHostedZoneResponse :: Int -> ChangeInfo -> AssociateVPCWithHostedZoneResponse
associateVPCWithHostedZoneResponse pStatus_ pChangeInfo_ =
    AssociateVPCWithHostedZoneResponse'
    { _avwhzrsStatus = pStatus_
    , _avwhzrsChangeInfo = pChangeInfo_
    }

-- | FIXME: Undocumented member.
avwhzrsStatus :: Lens' AssociateVPCWithHostedZoneResponse Int
avwhzrsStatus = lens _avwhzrsStatus (\ s a -> s{_avwhzrsStatus = a});

-- | A complex type that contains the ID, the status, and the date and time
-- of your @AssociateVPCWithHostedZoneRequest@.
avwhzrsChangeInfo :: Lens' AssociateVPCWithHostedZoneResponse ChangeInfo
avwhzrsChangeInfo = lens _avwhzrsChangeInfo (\ s a -> s{_avwhzrsChangeInfo = a});
