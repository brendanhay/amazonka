{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.SNS.V2010_03_31.RemovePermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes a statement from a topic's access control policy.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Test
-- &amp;Label=NewPermission &amp;Action=RemovePermission
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=N1abwRY9i7zaSQmbAlm71pPf9EEFOqNbQL1alzw2yCg%3D
-- &lt;RemovePermissionResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;d170b150-33a8-11df-995a-2d6fbe836cc1&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/RemovePermissionResponse&gt;.
module Network.AWS.SNS.V2010_03_31.RemovePermission where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

data RemovePermission = RemovePermission
    { _rpiLabel :: Text
      -- ^ The unique label of the statement you want to remove.
    , _rpiTopicArn :: Text
      -- ^ The ARN of the topic whose access control policy you wish to
      -- modify.
    } deriving (Show, Generic)

makeLenses ''RemovePermission

instance ToQuery RemovePermission where
    toQuery = genericQuery def

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Show, Generic)

makeLenses ''RemovePermissionResponse

instance AWSRequest RemovePermission where
    type Sv RemovePermission = SNS
    type Rs RemovePermission = RemovePermissionResponse

    request = post "RemovePermission"
    response _ = nullaryResponse RemovePermissionResponse
