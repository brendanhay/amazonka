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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified hosted zone.
--
--
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
    , uhzcrsResponseStatus
    , uhzcrsHostedZone
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to update the comment for a hosted zone.
--
--
--
-- /See:/ 'updateHostedZoneComment' smart constructor.
data UpdateHostedZoneComment = UpdateHostedZoneComment'
  { _uhzcComment :: !(Maybe Text)
  , _uhzcId      :: !ResourceId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateHostedZoneComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhzcComment' - The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
--
-- * 'uhzcId' - The ID for the hosted zone that you want to update the comment for.
updateHostedZoneComment
    :: ResourceId -- ^ 'uhzcId'
    -> UpdateHostedZoneComment
updateHostedZoneComment pId_ =
  UpdateHostedZoneComment' {_uhzcComment = Nothing, _uhzcId = pId_}


-- | The new comment for the hosted zone. If you don't specify a value for @Comment@ , Amazon Route 53 deletes the existing value of the @Comment@ element, if any.
uhzcComment :: Lens' UpdateHostedZoneComment (Maybe Text)
uhzcComment = lens _uhzcComment (\ s a -> s{_uhzcComment = a})

-- | The ID for the hosted zone that you want to update the comment for.
uhzcId :: Lens' UpdateHostedZoneComment ResourceId
uhzcId = lens _uhzcId (\ s a -> s{_uhzcId = a})

instance AWSRequest UpdateHostedZoneComment where
        type Rs UpdateHostedZoneComment =
             UpdateHostedZoneCommentResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 UpdateHostedZoneCommentResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HostedZone"))

instance Hashable UpdateHostedZoneComment where

instance NFData UpdateHostedZoneComment where

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

-- | A complex type that contains the response to the @UpdateHostedZoneComment@ request.
--
--
--
-- /See:/ 'updateHostedZoneCommentResponse' smart constructor.
data UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse'
  { _uhzcrsResponseStatus :: !Int
  , _uhzcrsHostedZone     :: !HostedZone
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateHostedZoneCommentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhzcrsResponseStatus' - -- | The response status code.
--
-- * 'uhzcrsHostedZone' - Undocumented member.
updateHostedZoneCommentResponse
    :: Int -- ^ 'uhzcrsResponseStatus'
    -> HostedZone -- ^ 'uhzcrsHostedZone'
    -> UpdateHostedZoneCommentResponse
updateHostedZoneCommentResponse pResponseStatus_ pHostedZone_ =
  UpdateHostedZoneCommentResponse'
    {_uhzcrsResponseStatus = pResponseStatus_, _uhzcrsHostedZone = pHostedZone_}


-- | -- | The response status code.
uhzcrsResponseStatus :: Lens' UpdateHostedZoneCommentResponse Int
uhzcrsResponseStatus = lens _uhzcrsResponseStatus (\ s a -> s{_uhzcrsResponseStatus = a})

-- | Undocumented member.
uhzcrsHostedZone :: Lens' UpdateHostedZoneCommentResponse HostedZone
uhzcrsHostedZone = lens _uhzcrsHostedZone (\ s a -> s{_uhzcrsHostedZone = a})

instance NFData UpdateHostedZoneCommentResponse where
