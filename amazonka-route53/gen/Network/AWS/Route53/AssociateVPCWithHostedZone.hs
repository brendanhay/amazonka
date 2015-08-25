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
-- Module      : Network.AWS.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action associates a VPC with an hosted zone.
--
-- To associate a VPC with an hosted zone, send a 'POST' request to the
-- '2013-04-01\/hostedzone\/hosted zone ID\/associatevpc' resource. The
-- request body must include an XML document with a
-- 'AssociateVPCWithHostedZoneRequest' element. The response returns the
-- 'AssociateVPCWithHostedZoneResponse' element that contains 'ChangeInfo'
-- for you to track the progress of the 'AssociateVPCWithHostedZoneRequest'
-- you made. See 'GetChange' operation for how to track the progress of
-- your change.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AWS API Reference> for AssociateVPCWithHostedZone.
module Network.AWS.Route53.AssociateVPCWithHostedZone
    (
    -- * Creating a Request
      associateVPCWithHostedZone
    , AssociateVPCWithHostedZone
    -- * Request Lenses
    , avwhzComment
    , avwhzHostedZoneId
    , avwhzVPC

    -- * Destructuring the Response
    , associateVPCWithHostedZoneResponse
    , AssociateVPCWithHostedZoneResponse
    -- * Response Lenses
    , avwhzrsStatus
    , avwhzrsChangeInfo
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to associate
-- a VPC with an hosted zone.
--
-- /See:/ 'associateVPCWithHostedZone' smart constructor.
data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone'
    { _avwhzComment      :: !(Maybe Text)
    , _avwhzHostedZoneId :: !Text
    , _avwhzVPC          :: !VPC
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociateVPCWithHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avwhzComment'
--
-- * 'avwhzHostedZoneId'
--
-- * 'avwhzVPC'
associateVPCWithHostedZone
    :: Text -- ^ 'avwhzHostedZoneId'
    -> VPC -- ^ 'avwhzVPC'
    -> AssociateVPCWithHostedZone
associateVPCWithHostedZone pHostedZoneId_ pVPC_ =
    AssociateVPCWithHostedZone'
    { _avwhzComment = Nothing
    , _avwhzHostedZoneId = pHostedZoneId_
    , _avwhzVPC = pVPC_
    }

-- | /Optional:/ Any comments you want to include about a
-- 'AssociateVPCWithHostedZoneRequest'.
avwhzComment :: Lens' AssociateVPCWithHostedZone (Maybe Text)
avwhzComment = lens _avwhzComment (\ s a -> s{_avwhzComment = a});

-- | The ID of the hosted zone you want to associate your VPC with.
--
-- Note that you cannot associate a VPC with a hosted zone that doesn\'t
-- have an existing VPC association.
avwhzHostedZoneId :: Lens' AssociateVPCWithHostedZone Text
avwhzHostedZoneId = lens _avwhzHostedZoneId (\ s a -> s{_avwhzHostedZoneId = a});

-- | The VPC that you want your hosted zone to be associated with.
avwhzVPC :: Lens' AssociateVPCWithHostedZone VPC
avwhzVPC = lens _avwhzVPC (\ s a -> s{_avwhzVPC = a});

instance AWSRequest AssociateVPCWithHostedZone where
        type Rs AssociateVPCWithHostedZone =
             AssociateVPCWithHostedZoneResponse
        request = postXML route53
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
              ["/2013-04-01/hostedzone/", toBS _avwhzHostedZoneId,
               "/associatevpc"]

instance ToQuery AssociateVPCWithHostedZone where
        toQuery = const mempty

instance ToXML AssociateVPCWithHostedZone where
        toXML AssociateVPCWithHostedZone'{..}
          = mconcat
              ["Comment" @= _avwhzComment, "VPC" @= _avwhzVPC]

-- | A complex type containing the response information for the request.
--
-- /See:/ 'associateVPCWithHostedZoneResponse' smart constructor.
data AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse'
    { _avwhzrsStatus     :: !Int
    , _avwhzrsChangeInfo :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociateVPCWithHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avwhzrsStatus'
--
-- * 'avwhzrsChangeInfo'
associateVPCWithHostedZoneResponse
    :: Int -- ^ 'avwhzrsStatus'
    -> ChangeInfo -- ^ 'avwhzrsChangeInfo'
    -> AssociateVPCWithHostedZoneResponse
associateVPCWithHostedZoneResponse pStatus_ pChangeInfo_ =
    AssociateVPCWithHostedZoneResponse'
    { _avwhzrsStatus = pStatus_
    , _avwhzrsChangeInfo = pChangeInfo_
    }

-- | The response status code.
avwhzrsStatus :: Lens' AssociateVPCWithHostedZoneResponse Int
avwhzrsStatus = lens _avwhzrsStatus (\ s a -> s{_avwhzrsStatus = a});

-- | A complex type that contains the ID, the status, and the date and time
-- of your 'AssociateVPCWithHostedZoneRequest'.
avwhzrsChangeInfo :: Lens' AssociateVPCWithHostedZoneResponse ChangeInfo
avwhzrsChangeInfo = lens _avwhzrsChangeInfo (\ s a -> s{_avwhzrsChangeInfo = a});
