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
-- Module      : Network.AWS.Shield.CreateProtection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables AWS Shield Advanced for a specific AWS resource. The resource can be an Amazon CloudFront distribution, Elastic Load Balancing load balancer, Elastic IP Address, or an Amazon Route 53 hosted zone.
--
--
module Network.AWS.Shield.CreateProtection
    (
    -- * Creating a Request
      createProtection
    , CreateProtection
    -- * Request Lenses
    , cpName
    , cpResourceARN

    -- * Destructuring the Response
    , createProtectionResponse
    , CreateProtectionResponse
    -- * Response Lenses
    , cprsProtectionId
    , cprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'createProtection' smart constructor.
data CreateProtection = CreateProtection'
  { _cpName        :: !Text
  , _cpResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpName' - Friendly name for the @Protection@ you are creating.
--
-- * 'cpResourceARN' - The ARN (Amazon Resource Name) of the resource to be protected. The ARN should be in one of the following formats:     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @      * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @      * For AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @      * For Amazon Route 53: @arn:aws:route53::/account-id/ :hostedzone//hosted-zone-id/ @      * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @
createProtection
    :: Text -- ^ 'cpName'
    -> Text -- ^ 'cpResourceARN'
    -> CreateProtection
createProtection pName_ pResourceARN_ =
  CreateProtection' {_cpName = pName_, _cpResourceARN = pResourceARN_}


-- | Friendly name for the @Protection@ you are creating.
cpName :: Lens' CreateProtection Text
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | The ARN (Amazon Resource Name) of the resource to be protected. The ARN should be in one of the following formats:     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @      * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @      * For AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @      * For Amazon Route 53: @arn:aws:route53::/account-id/ :hostedzone//hosted-zone-id/ @      * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @
cpResourceARN :: Lens' CreateProtection Text
cpResourceARN = lens _cpResourceARN (\ s a -> s{_cpResourceARN = a})

instance AWSRequest CreateProtection where
        type Rs CreateProtection = CreateProtectionResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 CreateProtectionResponse' <$>
                   (x .?> "ProtectionId") <*> (pure (fromEnum s)))

instance Hashable CreateProtection where

instance NFData CreateProtection where

instance ToHeaders CreateProtection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.CreateProtection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProtection where
        toJSON CreateProtection'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cpName),
                  Just ("ResourceArn" .= _cpResourceARN)])

instance ToPath CreateProtection where
        toPath = const "/"

instance ToQuery CreateProtection where
        toQuery = const mempty

-- | /See:/ 'createProtectionResponse' smart constructor.
data CreateProtectionResponse = CreateProtectionResponse'
  { _cprsProtectionId   :: !(Maybe Text)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProtectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProtectionId' - The unique identifier (ID) for the 'Protection' object that is created.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createProtectionResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreateProtectionResponse
createProtectionResponse pResponseStatus_ =
  CreateProtectionResponse'
    {_cprsProtectionId = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | The unique identifier (ID) for the 'Protection' object that is created.
cprsProtectionId :: Lens' CreateProtectionResponse (Maybe Text)
cprsProtectionId = lens _cprsProtectionId (\ s a -> s{_cprsProtectionId = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProtectionResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreateProtectionResponse where
