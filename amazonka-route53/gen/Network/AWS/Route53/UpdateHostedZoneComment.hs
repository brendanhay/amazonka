{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.UpdateHostedZoneComment
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

-- | To update the hosted zone comment, send a @POST@ request to the
-- @2013-04-01\/hostedzone\/hosted zone ID@ resource. The request body must
-- include an XML document with a @UpdateHostedZoneCommentRequest@ element.
-- The response to this request includes the modified @HostedZone@ element.
--
-- The comment can have a maximum length of 256 characters.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateHostedZoneComment.html>
module Network.AWS.Route53.UpdateHostedZoneComment
    (
    -- * Request
      UpdateHostedZoneComment
    -- ** Request constructor
    , updateHostedZoneComment
    -- ** Request lenses
    , uhzcComment
    , uhzcId

    -- * Response
    , UpdateHostedZoneCommentResponse
    -- ** Response constructor
    , updateHostedZoneCommentResponse
    -- ** Response lenses
    , uhzcrHostedZone
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types

-- | /See:/ 'updateHostedZoneComment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uhzcComment'
--
-- * 'uhzcId'
data UpdateHostedZoneComment = UpdateHostedZoneComment'{_uhzcComment :: Maybe Text, _uhzcId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateHostedZoneComment' smart constructor.
updateHostedZoneComment :: Text -> UpdateHostedZoneComment
updateHostedZoneComment pId = UpdateHostedZoneComment'{_uhzcComment = Nothing, _uhzcId = pId};

-- | A comment about your hosted zone.
uhzcComment :: Lens' UpdateHostedZoneComment (Maybe Text)
uhzcComment = lens _uhzcComment (\ s a -> s{_uhzcComment = a});

-- | The ID of the hosted zone you want to update.
uhzcId :: Lens' UpdateHostedZoneComment Text
uhzcId = lens _uhzcId (\ s a -> s{_uhzcId = a});

instance AWSRequest UpdateHostedZoneComment where
        type Sv UpdateHostedZoneComment = Route53
        type Rs UpdateHostedZoneComment =
             UpdateHostedZoneCommentResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateHostedZoneCommentResponse' <$>
                   (x .@ "HostedZone"))

instance ToElement UpdateHostedZoneComment where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateHostedZoneCommentRequest"

instance ToHeaders UpdateHostedZoneComment where
        toHeaders = const mempty

instance ToPath UpdateHostedZoneComment where
        toPath UpdateHostedZoneComment'{..}
          = mconcat ["/2013-04-01/hostedzone/", toText _uhzcId]

instance ToQuery UpdateHostedZoneComment where
        toQuery = const mempty

instance ToXML UpdateHostedZoneComment where
        toXML UpdateHostedZoneComment'{..}
          = mconcat ["Comment" @= _uhzcComment]

-- | /See:/ 'updateHostedZoneCommentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uhzcrHostedZone'
newtype UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'{_uhzcrHostedZone :: HostedZone} deriving (Eq, Read, Show)

-- | 'UpdateHostedZoneCommentResponse' smart constructor.
updateHostedZoneCommentResponse :: HostedZone -> UpdateHostedZoneCommentResponse
updateHostedZoneCommentResponse pHostedZone = UpdateHostedZoneCommentResponse'{_uhzcrHostedZone = pHostedZone};

-- | FIXME: Undocumented member.
uhzcrHostedZone :: Lens' UpdateHostedZoneCommentResponse HostedZone
uhzcrHostedZone = lens _uhzcrHostedZone (\ s a -> s{_uhzcrHostedZone = a});
