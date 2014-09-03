{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.GetPasswordData
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
module Network.AWS.EC2.V2014_06_15.GetPasswordData
    (
    -- * Request
      GetPasswordData
    -- ** Request constructor
    , getPasswordData
    -- ** Request lenses
    , gpdrInstanceId

    -- * Response
    , GetPasswordDataResponse
    -- ** Response lenses
    , gpdsTimestamp
    , gpdsInstanceId
    , gpdsPasswordData
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetPasswordData' request.
getPasswordData :: Text -- ^ 'gpdrInstanceId'
                -> GetPasswordData
getPasswordData p1 = GetPasswordData
    { _gpdrInstanceId = p1
    }

data GetPasswordData = GetPasswordData
    { _gpdrInstanceId :: Text
      -- ^ The ID of the Windows instance.
    } deriving (Show, Generic)

-- | The ID of the Windows instance.
gpdrInstanceId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetPasswordData
    -> f GetPasswordData
gpdrInstanceId f x =
    (\y -> x { _gpdrInstanceId = y })
       <$> f (_gpdrInstanceId x)
{-# INLINE gpdrInstanceId #-}

instance ToQuery GetPasswordData where
    toQuery = genericQuery def

data GetPasswordDataResponse = GetPasswordDataResponse
    { _gpdsTimestamp :: Maybe ISO8601
      -- ^ The time the data was last updated.
    , _gpdsInstanceId :: Maybe Text
      -- ^ The ID of the Windows instance.
    , _gpdsPasswordData :: Maybe Text
      -- ^ The password of the instance.
    } deriving (Show, Generic)

-- | The time the data was last updated.
gpdsTimestamp
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetPasswordDataResponse
    -> f GetPasswordDataResponse
gpdsTimestamp f x =
    (\y -> x { _gpdsTimestamp = y })
       <$> f (_gpdsTimestamp x)
{-# INLINE gpdsTimestamp #-}

-- | The ID of the Windows instance.
gpdsInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetPasswordDataResponse
    -> f GetPasswordDataResponse
gpdsInstanceId f x =
    (\y -> x { _gpdsInstanceId = y })
       <$> f (_gpdsInstanceId x)
{-# INLINE gpdsInstanceId #-}

-- | The password of the instance.
gpdsPasswordData
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetPasswordDataResponse
    -> f GetPasswordDataResponse
gpdsPasswordData f x =
    (\y -> x { _gpdsPasswordData = y })
       <$> f (_gpdsPasswordData x)
{-# INLINE gpdsPasswordData #-}

instance FromXML GetPasswordDataResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetPasswordData where
    type Sv GetPasswordData = EC2
    type Rs GetPasswordData = GetPasswordDataResponse

    request = post "GetPasswordData"
    response _ = xmlResponse
