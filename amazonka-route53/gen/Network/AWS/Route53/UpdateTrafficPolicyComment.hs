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
-- Module      : Network.AWS.Route53.UpdateTrafficPolicyComment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the comment for a specified traffic policy version.
--
-- To update the comment, send a 'POST' request to the
-- '\/2013-04-01\/trafficpolicy\/' resource.
--
-- The request body must include an XML document with an
-- 'UpdateTrafficPolicyCommentRequest' element.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_UpdateTrafficPolicyComment.html AWS API Reference> for UpdateTrafficPolicyComment.
module Network.AWS.Route53.UpdateTrafficPolicyComment
    (
    -- * Creating a Request
      updateTrafficPolicyComment
    , UpdateTrafficPolicyComment
    -- * Request Lenses
    , utpcId
    , utpcVersion
    , utpcComment

    -- * Destructuring the Response
    , updateTrafficPolicyCommentResponse
    , UpdateTrafficPolicyCommentResponse
    -- * Response Lenses
    , utpcrsResponseStatus
    , utpcrsTrafficPolicy
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the traffic policy for
-- which you want to update the comment.
--
-- /See:/ 'updateTrafficPolicyComment' smart constructor.
data UpdateTrafficPolicyComment = UpdateTrafficPolicyComment'
    { _utpcId      :: !Text
    , _utpcVersion :: !Nat
    , _utpcComment :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateTrafficPolicyComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utpcId'
--
-- * 'utpcVersion'
--
-- * 'utpcComment'
updateTrafficPolicyComment
    :: Text -- ^ 'utpcId'
    -> Natural -- ^ 'utpcVersion'
    -> Text -- ^ 'utpcComment'
    -> UpdateTrafficPolicyComment
updateTrafficPolicyComment pId_ pVersion_ pComment_ =
    UpdateTrafficPolicyComment'
    { _utpcId = pId_
    , _utpcVersion = _Nat # pVersion_
    , _utpcComment = pComment_
    }

-- | The value of 'Id' for the traffic policy for which you want to update
-- the comment.
utpcId :: Lens' UpdateTrafficPolicyComment Text
utpcId = lens _utpcId (\ s a -> s{_utpcId = a});

-- | The value of 'Version' for the traffic policy for which you want to
-- update the comment.
utpcVersion :: Lens' UpdateTrafficPolicyComment Natural
utpcVersion = lens _utpcVersion (\ s a -> s{_utpcVersion = a}) . _Nat;

-- | The new comment for the specified traffic policy and version.
utpcComment :: Lens' UpdateTrafficPolicyComment Text
utpcComment = lens _utpcComment (\ s a -> s{_utpcComment = a});

instance AWSRequest UpdateTrafficPolicyComment where
        type Rs UpdateTrafficPolicyComment =
             UpdateTrafficPolicyCommentResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 UpdateTrafficPolicyCommentResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "TrafficPolicy"))

instance ToElement UpdateTrafficPolicyComment where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}UpdateTrafficPolicyCommentRequest"

instance ToHeaders UpdateTrafficPolicyComment where
        toHeaders = const mempty

instance ToPath UpdateTrafficPolicyComment where
        toPath UpdateTrafficPolicyComment'{..}
          = mconcat
              ["/2013-04-01/trafficpolicy/", toBS _utpcId, "/",
               toBS _utpcVersion]

instance ToQuery UpdateTrafficPolicyComment where
        toQuery = const mempty

instance ToXML UpdateTrafficPolicyComment where
        toXML UpdateTrafficPolicyComment'{..}
          = mconcat ["Comment" @= _utpcComment]

-- | A complex type that contains the response information for the traffic
-- policy.
--
-- /See:/ 'updateTrafficPolicyCommentResponse' smart constructor.
data UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse'
    { _utpcrsResponseStatus :: !Int
    , _utpcrsTrafficPolicy  :: !TrafficPolicy
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateTrafficPolicyCommentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utpcrsResponseStatus'
--
-- * 'utpcrsTrafficPolicy'
updateTrafficPolicyCommentResponse
    :: Int -- ^ 'utpcrsResponseStatus'
    -> TrafficPolicy -- ^ 'utpcrsTrafficPolicy'
    -> UpdateTrafficPolicyCommentResponse
updateTrafficPolicyCommentResponse pResponseStatus_ pTrafficPolicy_ =
    UpdateTrafficPolicyCommentResponse'
    { _utpcrsResponseStatus = pResponseStatus_
    , _utpcrsTrafficPolicy = pTrafficPolicy_
    }

-- | The response status code.
utpcrsResponseStatus :: Lens' UpdateTrafficPolicyCommentResponse Int
utpcrsResponseStatus = lens _utpcrsResponseStatus (\ s a -> s{_utpcrsResponseStatus = a});

-- | A complex type that contains settings for the specified traffic policy.
utpcrsTrafficPolicy :: Lens' UpdateTrafficPolicyCommentResponse TrafficPolicy
utpcrsTrafficPolicy = lens _utpcrsTrafficPolicy (\ s a -> s{_utpcrsTrafficPolicy = a});
