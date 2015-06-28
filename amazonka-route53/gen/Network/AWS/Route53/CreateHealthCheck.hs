{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.CreateHealthCheck
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

-- | This action creates a new health check.
--
-- To create a new health check, send a @POST@ request to the
-- @2013-04-01\/healthcheck@ resource. The request body must include an XML
-- document with a @CreateHealthCheckRequest@ element. The response returns
-- the @CreateHealthCheckResponse@ element that contains metadata about the
-- health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHealthCheck.html>
module Network.AWS.Route53.CreateHealthCheck
    (
    -- * Request
      CreateHealthCheck
    -- ** Request constructor
    , createHealthCheck
    -- ** Request lenses
    , chcCallerReference
    , chcHealthCheckConfig

    -- * Response
    , CreateHealthCheckResponse
    -- ** Response constructor
    , createHealthCheckResponse
    -- ** Response lenses
    , chcrHealthCheck
    , chcrLocation
    , chcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | >A complex type that contains information about the request to create a
-- health check.
--
-- /See:/ 'createHealthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcCallerReference'
--
-- * 'chcHealthCheckConfig'
data CreateHealthCheck = CreateHealthCheck'
    { _chcCallerReference   :: !Text
    , _chcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq,Read,Show)

-- | 'CreateHealthCheck' smart constructor.
createHealthCheck :: Text -> HealthCheckConfig -> CreateHealthCheck
createHealthCheck pCallerReference pHealthCheckConfig =
    CreateHealthCheck'
    { _chcCallerReference = pCallerReference
    , _chcHealthCheckConfig = pHealthCheckConfig
    }

-- | A unique string that identifies the request and that allows failed
-- @CreateHealthCheck@ requests to be retried without the risk of executing
-- the operation twice. You must use a unique @CallerReference@ string
-- every time you create a health check. @CallerReference@ can be any
-- unique string; you might choose to use a string that identifies your
-- project.
--
-- Valid characters are any Unicode code points that are legal in an XML
-- 1.0 document. The UTF-8 encoding of the value must be less than 128
-- bytes.
chcCallerReference :: Lens' CreateHealthCheck Text
chcCallerReference = lens _chcCallerReference (\ s a -> s{_chcCallerReference = a});

-- | A complex type that contains health check configuration.
chcHealthCheckConfig :: Lens' CreateHealthCheck HealthCheckConfig
chcHealthCheckConfig = lens _chcHealthCheckConfig (\ s a -> s{_chcHealthCheckConfig = a});

instance AWSRequest CreateHealthCheck where
        type Sv CreateHealthCheck = Route53
        type Rs CreateHealthCheck = CreateHealthCheckResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateHealthCheckResponse' <$>
                   (x .@ "HealthCheck") <*> (h .# "Location") <*>
                     (pure s))

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

-- | A complex type containing the response information for the new health
-- check.
--
-- /See:/ 'createHealthCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHealthCheck'
--
-- * 'chcrLocation'
--
-- * 'chcrStatus'
data CreateHealthCheckResponse = CreateHealthCheckResponse'
    { _chcrHealthCheck :: !HealthCheck
    , _chcrLocation    :: !Text
    , _chcrStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateHealthCheckResponse' smart constructor.
createHealthCheckResponse :: HealthCheck -> Text -> Status -> CreateHealthCheckResponse
createHealthCheckResponse pHealthCheck pLocation pStatus =
    CreateHealthCheckResponse'
    { _chcrHealthCheck = pHealthCheck
    , _chcrLocation = pLocation
    , _chcrStatus = pStatus
    }

-- | A complex type that contains identifying information about the health
-- check.
chcrHealthCheck :: Lens' CreateHealthCheckResponse HealthCheck
chcrHealthCheck = lens _chcrHealthCheck (\ s a -> s{_chcrHealthCheck = a});

-- | The unique URL representing the new health check.
chcrLocation :: Lens' CreateHealthCheckResponse Text
chcrLocation = lens _chcrLocation (\ s a -> s{_chcrLocation = a});

-- | FIXME: Undocumented member.
chcrStatus :: Lens' CreateHealthCheckResponse Status
chcrStatus = lens _chcrStatus (\ s a -> s{_chcrStatus = a});
