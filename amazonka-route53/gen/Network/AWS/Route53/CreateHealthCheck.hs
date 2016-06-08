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
-- Module      : Network.AWS.Route53.CreateHealthCheck
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action creates a new health check.
--
-- To create a new health check, send a 'POST' request to the '\/Route 53 API version\/healthcheck' resource. The request body must include a document with a 'CreateHealthCheckRequest' element. The response returns the 'CreateHealthCheckResponse' element that contains metadata about the health check.
module Network.AWS.Route53.CreateHealthCheck
    (
    -- * Creating a Request
      createHealthCheck
    , CreateHealthCheck
    -- * Request Lenses
    , chcCallerReference
    , chcHealthCheckConfig

    -- * Destructuring the Response
    , createHealthCheckResponse
    , CreateHealthCheckResponse
    -- * Response Lenses
    , chcrsResponseStatus
    , chcrsHealthCheck
    , chcrsLocation
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | >A complex type that contains information about the request to create a health check.
--
-- /See:/ 'createHealthCheck' smart constructor.
data CreateHealthCheck = CreateHealthCheck'
    { _chcCallerReference   :: !Text
    , _chcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcCallerReference'
--
-- * 'chcHealthCheckConfig'
createHealthCheck
    :: Text -- ^ 'chcCallerReference'
    -> HealthCheckConfig -- ^ 'chcHealthCheckConfig'
    -> CreateHealthCheck
createHealthCheck pCallerReference_ pHealthCheckConfig_ =
    CreateHealthCheck'
    { _chcCallerReference = pCallerReference_
    , _chcHealthCheckConfig = pHealthCheckConfig_
    }

-- | A unique string that identifies the request and that allows failed 'CreateHealthCheck' requests to be retried without the risk of executing the operation twice. You must use a unique 'CallerReference' string every time you create a health check. 'CallerReference' can be any unique string; you might choose to use a string that identifies your project.
--
-- Valid characters are any Unicode code points that are legal in an XML 1.0 document. The UTF-8 encoding of the value must be less than 128 bytes.
chcCallerReference :: Lens' CreateHealthCheck Text
chcCallerReference = lens _chcCallerReference (\ s a -> s{_chcCallerReference = a});

-- | A complex type that contains health check configuration.
chcHealthCheckConfig :: Lens' CreateHealthCheck HealthCheckConfig
chcHealthCheckConfig = lens _chcHealthCheckConfig (\ s a -> s{_chcHealthCheckConfig = a});

instance AWSRequest CreateHealthCheck where
        type Rs CreateHealthCheck = CreateHealthCheckResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateHealthCheckResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HealthCheck") <*>
                     (h .# "Location"))

instance Hashable CreateHealthCheck

instance NFData CreateHealthCheck

instance ToElement CreateHealthCheck where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHealthCheckRequest"

instance ToHeaders CreateHealthCheck where
        toHeaders = const mempty

instance ToPath CreateHealthCheck where
        toPath = const "/2013-04-01/healthcheck"

instance ToQuery CreateHealthCheck where
        toQuery = const mempty

instance ToXML CreateHealthCheck where
        toXML CreateHealthCheck'{..}
          = mconcat
              ["CallerReference" @= _chcCallerReference,
               "HealthCheckConfig" @= _chcHealthCheckConfig]

-- | A complex type containing the response information for the new health check.
--
-- /See:/ 'createHealthCheckResponse' smart constructor.
data CreateHealthCheckResponse = CreateHealthCheckResponse'
    { _chcrsResponseStatus :: !Int
    , _chcrsHealthCheck    :: !HealthCheck
    , _chcrsLocation       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcrsResponseStatus'
--
-- * 'chcrsHealthCheck'
--
-- * 'chcrsLocation'
createHealthCheckResponse
    :: Int -- ^ 'chcrsResponseStatus'
    -> HealthCheck -- ^ 'chcrsHealthCheck'
    -> Text -- ^ 'chcrsLocation'
    -> CreateHealthCheckResponse
createHealthCheckResponse pResponseStatus_ pHealthCheck_ pLocation_ =
    CreateHealthCheckResponse'
    { _chcrsResponseStatus = pResponseStatus_
    , _chcrsHealthCheck = pHealthCheck_
    , _chcrsLocation = pLocation_
    }

-- | The response status code.
chcrsResponseStatus :: Lens' CreateHealthCheckResponse Int
chcrsResponseStatus = lens _chcrsResponseStatus (\ s a -> s{_chcrsResponseStatus = a});

-- | A complex type that contains identifying information about the health check.
chcrsHealthCheck :: Lens' CreateHealthCheckResponse HealthCheck
chcrsHealthCheck = lens _chcrsHealthCheck (\ s a -> s{_chcrsHealthCheck = a});

-- | The unique URL representing the new health check.
chcrsLocation :: Lens' CreateHealthCheckResponse Text
chcrsLocation = lens _chcrsLocation (\ s a -> s{_chcrsLocation = a});

instance NFData CreateHealthCheckResponse
