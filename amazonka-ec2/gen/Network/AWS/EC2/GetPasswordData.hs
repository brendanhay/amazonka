{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the encrypted administrator password for an instance running
-- Windows. The Windows password is only generated the first time an AMI is
-- launched. It is not generated for rebundled AMIs or after the password is
-- changed on an instance. The password is encrypted using the key pair that
-- you specified when you launched the instance. You must provide the
-- corresponding key pair file. Password generation and encryption takes a few
-- moments. We recommend that you wait up to 15 minutes after launching an
-- instance before trying to retrieve the generated password. Example This
-- example returns the encrypted version of the administrator password for the
-- specified instance. https://ec2.amazonaws.com/?Action=GetPasswordData
-- &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS &lt;GetPasswordDataResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;instanceId&gt;i-2574e22a&lt;/instanceId&gt; &lt;timestamp&gt;2009-10-24
-- 15:00:00&lt;/timestamp&gt;
-- &lt;passwordData&gt;TGludXggdmVyc2lvbiAyLjYuMTYteGVuVSAoYnVpbGRlckBwYXRjaGJhdC5hbWF6b25zYSkgKGdj&lt;/passwordData&gt;
-- &lt;/GetPasswordDataResponse&gt;.
module Network.AWS.EC2.GetPasswordData
    (
    -- * Request
      GetPasswordData
    -- ** Request constructor
    , mkGetPasswordData
    -- ** Request lenses
    , gpdInstanceId

    -- * Response
    , GetPasswordDataResponse
    -- ** Response constructor
    , mkGetPasswordDataResponse
    -- ** Response lenses
    , gpdrInstanceId
    , gpdrTimestamp
    , gpdrPasswordData
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype GetPasswordData = GetPasswordData
    { _gpdInstanceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetPasswordData' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
mkGetPasswordData :: Text -- ^ 'gpdInstanceId'
                  -> GetPasswordData
mkGetPasswordData p1 = GetPasswordData
    { _gpdInstanceId = p1
    }

-- | The ID of the Windows instance.
gpdInstanceId :: Lens' GetPasswordData Text
gpdInstanceId = lens _gpdInstanceId (\s a -> s { _gpdInstanceId = a })

instance ToQuery GetPasswordData where
    toQuery = genericQuery def

data GetPasswordDataResponse = GetPasswordDataResponse
    { _gpdrInstanceId :: !(Maybe Text)
    , _gpdrTimestamp :: !(Maybe ISO8601)
    , _gpdrPasswordData :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetPasswordDataResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Timestamp ::@ @Maybe ISO8601@
--
-- * @PasswordData ::@ @Maybe Text@
--
mkGetPasswordDataResponse :: GetPasswordDataResponse
mkGetPasswordDataResponse = GetPasswordDataResponse
    { _gpdrInstanceId = Nothing
    , _gpdrTimestamp = Nothing
    , _gpdrPasswordData = Nothing
    }

-- | The ID of the Windows instance.
gpdrInstanceId :: Lens' GetPasswordDataResponse (Maybe Text)
gpdrInstanceId = lens _gpdrInstanceId (\s a -> s { _gpdrInstanceId = a })

-- | The time the data was last updated.
gpdrTimestamp :: Lens' GetPasswordDataResponse (Maybe ISO8601)
gpdrTimestamp = lens _gpdrTimestamp (\s a -> s { _gpdrTimestamp = a })

-- | The password of the instance.
gpdrPasswordData :: Lens' GetPasswordDataResponse (Maybe Text)
gpdrPasswordData =
    lens _gpdrPasswordData (\s a -> s { _gpdrPasswordData = a })

instance FromXML GetPasswordDataResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetPasswordData where
    type Sv GetPasswordData = EC2
    type Rs GetPasswordData = GetPasswordDataResponse

    request = post "GetPasswordData"
    response _ = xmlResponse
