{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.V2010_03_31.DeletePlatformApplication
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
module Network.AWS.SNS.V2010_03_31.DeletePlatformApplication where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

data DeletePlatformApplication = DeletePlatformApplication
    { _dpaiPlatformApplicationArn :: Text
      -- ^ PlatformApplicationArn of platform application object to delete.
    } deriving (Show, Generic)

makeLenses ''DeletePlatformApplication

instance ToQuery DeletePlatformApplication where
    toQuery = genericToQuery def

data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeletePlatformApplicationResponse

instance AWSRequest DeletePlatformApplication where
    type Sv DeletePlatformApplication = SNS
    type Rs DeletePlatformApplication = DeletePlatformApplicationResponse

    request = post "DeletePlatformApplication"
    response _ _ = return (Right DeletePlatformApplicationResponse)
