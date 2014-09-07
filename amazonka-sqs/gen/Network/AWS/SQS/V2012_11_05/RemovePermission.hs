{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.RemovePermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes any permissions in the queue policy that matches the specified
-- Label parameter. Only the owner of the queue can remove permissions. The
-- following example Query request removes the testLabel permission on the
-- queue named testQueue.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=RemovePermission &Label=testLabel &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- f8bdb362-6616-42c0-977a-ce9a8bcce3bb.
module Network.AWS.SQS.V2012_11_05.RemovePermission
    (
    -- * Request
      RemovePermission
    -- ** Request constructor
    , mkRemovePermission
    -- ** Request lenses
    , rpQueueUrl
    , rpLabel

    -- * Response
    , RemovePermissionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

-- | 
data RemovePermission = RemovePermission
    { _rpQueueUrl :: Text
    , _rpLabel :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemovePermission' request.
mkRemovePermission :: Text -- ^ 'rpQueueUrl'
                   -> Text -- ^ 'rpLabel'
                   -> RemovePermission
mkRemovePermission p1 p2 = RemovePermission
    { _rpQueueUrl = p1
    , _rpLabel = p2
    }

-- | The URL of the Amazon SQS queue to take action on.
rpQueueUrl :: Lens' RemovePermission Text
rpQueueUrl = lens _rpQueueUrl (\s a -> s { _rpQueueUrl = a })

-- | The identification of the permission to remove. This is the label added
-- with the AddPermission action.
rpLabel :: Lens' RemovePermission Text
rpLabel = lens _rpLabel (\s a -> s { _rpLabel = a })

instance ToQuery RemovePermission where
    toQuery = genericQuery def

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemovePermission where
    type Sv RemovePermission = SQS
    type Rs RemovePermission = RemovePermissionResponse

    request = post "RemovePermission"
    response _ = nullaryResponse RemovePermissionResponse
