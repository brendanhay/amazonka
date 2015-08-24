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
-- Module      : Network.AWS.Route53.UpdateHostedZoneComment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To update the hosted zone comment, send a 'POST' request to the
-- '2013-04-01\/hostedzone\/hosted zone ID' resource. The request body must
-- include an XML document with a 'UpdateHostedZoneCommentRequest' element.
-- The response to this request includes the modified 'HostedZone' element.
--
-- The comment can have a maximum length of 256 characters.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHostedZoneComment.html AWS API Reference> for UpdateHostedZoneComment.
module Network.AWS.Route53.UpdateHostedZoneComment
    (
    -- * Creating a Request
      updateHostedZoneComment
    , UpdateHostedZoneComment
    -- * Request Lenses
    , uhzcComment
    , uhzcId

    -- * Destructuring the Response
    , updateHostedZoneCommentResponse
    , UpdateHostedZoneCommentResponse
    -- * Response Lenses
    , uhzcrsStatus
    , uhzcrsHostedZone
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to update a
-- hosted zone comment.
--
-- /See:/ 'updateHostedZoneComment' smart constructor.
data UpdateHostedZoneComment = UpdateHostedZoneComment'
    { _uhzcComment :: !(Maybe Text)
    , _uhzcId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateHostedZoneComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhzcComment'
--
-- * 'uhzcId'
updateHostedZoneComment
    :: Text -- ^ 'uhzcId'
    -> UpdateHostedZoneComment
updateHostedZoneComment pId_ =
    UpdateHostedZoneComment'
    { _uhzcComment = Nothing
    , _uhzcId = pId_
    }

-- | A comment about your hosted zone.
uhzcComment :: Lens' UpdateHostedZoneComment (Maybe Text)
uhzcComment = lens _uhzcComment (\ s a -> s{_uhzcComment = a});

-- | The ID of the hosted zone you want to update.
uhzcId :: Lens' UpdateHostedZoneComment Text
uhzcId = lens _uhzcId (\ s a -> s{_uhzcId = a});

instance AWSRequest UpdateHostedZoneComment where
        type Rs UpdateHostedZoneComment =
             UpdateHostedZoneCommentResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 UpdateHostedZoneCommentResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HostedZone"))

instance ToElement UpdateHostedZoneComment where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHostedZoneCommentRequest"

instance ToHeaders UpdateHostedZoneComment where
        toHeaders = const mempty

instance ToPath UpdateHostedZoneComment where
        toPath UpdateHostedZoneComment'{..}
          = mconcat ["/2013-04-01/hostedzone/", toBS _uhzcId]

instance ToQuery UpdateHostedZoneComment where
        toQuery = const mempty

instance ToXML UpdateHostedZoneComment where
        toXML UpdateHostedZoneComment'{..}
          = mconcat ["Comment" @= _uhzcComment]

-- | A complex type containing information about the specified hosted zone
-- after the update.
--
-- /See:/ 'updateHostedZoneCommentResponse' smart constructor.
data UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'
    { _uhzcrsStatus     :: !Int
    , _uhzcrsHostedZone :: !HostedZone
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateHostedZoneCommentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhzcrsStatus'
--
-- * 'uhzcrsHostedZone'
updateHostedZoneCommentResponse
    :: Int -- ^ 'uhzcrsStatus'
    -> HostedZone -- ^ 'uhzcrsHostedZone'
    -> UpdateHostedZoneCommentResponse
updateHostedZoneCommentResponse pStatus_ pHostedZone_ =
    UpdateHostedZoneCommentResponse'
    { _uhzcrsStatus = pStatus_
    , _uhzcrsHostedZone = pHostedZone_
    }

-- | The response status code.
uhzcrsStatus :: Lens' UpdateHostedZoneCommentResponse Int
uhzcrsStatus = lens _uhzcrsStatus (\ s a -> s{_uhzcrsStatus = a});

-- | Undocumented member.
uhzcrsHostedZone :: Lens' UpdateHostedZoneCommentResponse HostedZone
uhzcrsHostedZone = lens _uhzcrsHostedZone (\ s a -> s{_uhzcrsHostedZone = a});
