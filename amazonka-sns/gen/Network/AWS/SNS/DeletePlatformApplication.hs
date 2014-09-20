{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a platform application object for one of the supported push
-- notification services, such as APNS and GCM. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=DeletePlatformApplication &amp;SignatureMethod=HmacSHA256
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &amp;SignatureVersion=2
-- &amp;Version=2010-03-31
-- &amp;Signature=Mh7X%2BQo%2BGpcm5B1IpkovBaRiJCJOqvFlIOYzL62SGrg%3D
-- &amp;Timestamp=2013-07-01T23%3A02%3A03.872Z HTTP/1.1 200 OK ...
-- &lt;DeletePlatformApplicationResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;097dac18-7a77-5823-a8dd-e65476dcb037&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/DeletePlatformApplicationResponse&gt;.
module Network.AWS.SNS.DeletePlatformApplication
    (
    -- * Request
      DeletePlatformApplication
    -- ** Request constructor
    , deletePlatformApplication
    -- ** Request lenses
    , dpaPlatformApplicationArn

    -- * Response
    , DeletePlatformApplicationResponse
    -- ** Response constructor
    , deletePlatformApplicationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for DeletePlatformApplication action.
newtype DeletePlatformApplication = DeletePlatformApplication
    { _dpaPlatformApplicationArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePlatformApplication' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PlatformApplicationArn ::@ @Text@
--
deletePlatformApplication :: Text -- ^ 'dpaPlatformApplicationArn'
                          -> DeletePlatformApplication
deletePlatformApplication p1 = DeletePlatformApplication
    { _dpaPlatformApplicationArn = p1
    }

-- | PlatformApplicationArn of platform application object to delete.
dpaPlatformApplicationArn :: Lens' DeletePlatformApplication Text
dpaPlatformApplicationArn =
    lens _dpaPlatformApplicationArn
         (\s a -> s { _dpaPlatformApplicationArn = a })

instance ToQuery DeletePlatformApplication where
    toQuery = genericQuery def

data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePlatformApplicationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deletePlatformApplicationResponse :: DeletePlatformApplicationResponse
deletePlatformApplicationResponse = DeletePlatformApplicationResponse

instance AWSRequest DeletePlatformApplication where
    type Sv DeletePlatformApplication = SNS
    type Rs DeletePlatformApplication = DeletePlatformApplicationResponse

    request = post "DeletePlatformApplication"
    response _ = nullaryResponse DeletePlatformApplicationResponse
