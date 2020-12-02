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
-- Module      : Network.AWS.Route53.CreateTrafficPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a traffic policy, which you use to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com).
--
--
module Network.AWS.Route53.CreateTrafficPolicy
    (
    -- * Creating a Request
      createTrafficPolicy
    , CreateTrafficPolicy
    -- * Request Lenses
    , ctpComment
    , ctpName
    , ctpDocument

    -- * Destructuring the Response
    , createTrafficPolicyResponse
    , CreateTrafficPolicyResponse
    -- * Response Lenses
    , ctprsResponseStatus
    , ctprsTrafficPolicy
    , ctprsLocation
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the traffic policy that you want to create.
--
--
--
-- /See:/ 'createTrafficPolicy' smart constructor.
data CreateTrafficPolicy = CreateTrafficPolicy'
  { _ctpComment  :: !(Maybe Text)
  , _ctpName     :: !Text
  , _ctpDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrafficPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpComment' - (Optional) Any comments that you want to include about the traffic policy.
--
-- * 'ctpName' - The name of the traffic policy.
--
-- * 'ctpDocument' - The definition of this traffic policy in JSON format. For more information, see <http://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
createTrafficPolicy
    :: Text -- ^ 'ctpName'
    -> Text -- ^ 'ctpDocument'
    -> CreateTrafficPolicy
createTrafficPolicy pName_ pDocument_ =
  CreateTrafficPolicy'
    {_ctpComment = Nothing, _ctpName = pName_, _ctpDocument = pDocument_}


-- | (Optional) Any comments that you want to include about the traffic policy.
ctpComment :: Lens' CreateTrafficPolicy (Maybe Text)
ctpComment = lens _ctpComment (\ s a -> s{_ctpComment = a})

-- | The name of the traffic policy.
ctpName :: Lens' CreateTrafficPolicy Text
ctpName = lens _ctpName (\ s a -> s{_ctpName = a})

-- | The definition of this traffic policy in JSON format. For more information, see <http://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
ctpDocument :: Lens' CreateTrafficPolicy Text
ctpDocument = lens _ctpDocument (\ s a -> s{_ctpDocument = a})

instance AWSRequest CreateTrafficPolicy where
        type Rs CreateTrafficPolicy =
             CreateTrafficPolicyResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateTrafficPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "TrafficPolicy") <*>
                     (h .# "Location"))

instance Hashable CreateTrafficPolicy where

instance NFData CreateTrafficPolicy where

instance ToElement CreateTrafficPolicy where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyRequest"

instance ToHeaders CreateTrafficPolicy where
        toHeaders = const mempty

instance ToPath CreateTrafficPolicy where
        toPath = const "/2013-04-01/trafficpolicy"

instance ToQuery CreateTrafficPolicy where
        toQuery = const mempty

instance ToXML CreateTrafficPolicy where
        toXML CreateTrafficPolicy'{..}
          = mconcat
              ["Comment" @= _ctpComment, "Name" @= _ctpName,
               "Document" @= _ctpDocument]

-- | A complex type that contains the response information for the @CreateTrafficPolicy@ request.
--
--
--
-- /See:/ 'createTrafficPolicyResponse' smart constructor.
data CreateTrafficPolicyResponse = CreateTrafficPolicyResponse'
  { _ctprsResponseStatus :: !Int
  , _ctprsTrafficPolicy  :: !TrafficPolicy
  , _ctprsLocation       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrafficPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctprsResponseStatus' - -- | The response status code.
--
-- * 'ctprsTrafficPolicy' - A complex type that contains settings for the new traffic policy.
--
-- * 'ctprsLocation' - A unique URL that represents a new traffic policy.
createTrafficPolicyResponse
    :: Int -- ^ 'ctprsResponseStatus'
    -> TrafficPolicy -- ^ 'ctprsTrafficPolicy'
    -> Text -- ^ 'ctprsLocation'
    -> CreateTrafficPolicyResponse
createTrafficPolicyResponse pResponseStatus_ pTrafficPolicy_ pLocation_ =
  CreateTrafficPolicyResponse'
    { _ctprsResponseStatus = pResponseStatus_
    , _ctprsTrafficPolicy = pTrafficPolicy_
    , _ctprsLocation = pLocation_
    }


-- | -- | The response status code.
ctprsResponseStatus :: Lens' CreateTrafficPolicyResponse Int
ctprsResponseStatus = lens _ctprsResponseStatus (\ s a -> s{_ctprsResponseStatus = a})

-- | A complex type that contains settings for the new traffic policy.
ctprsTrafficPolicy :: Lens' CreateTrafficPolicyResponse TrafficPolicy
ctprsTrafficPolicy = lens _ctprsTrafficPolicy (\ s a -> s{_ctprsTrafficPolicy = a})

-- | A unique URL that represents a new traffic policy.
ctprsLocation :: Lens' CreateTrafficPolicyResponse Text
ctprsLocation = lens _ctprsLocation (\ s a -> s{_ctprsLocation = a})

instance NFData CreateTrafficPolicyResponse where
