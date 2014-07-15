{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.V2006_03_01.Lenses where

import Control.Lens.TH
import Network.AWS.S3.V2006_03_01.Types

makeIso ''BucketLoggingStatus
makeIso ''CORSConfiguration
makeIso ''CommonPrefix
makeIso ''CompletedMultipartUpload
makeIso ''CreateBucketConfiguration
makeIso ''ErrorDocument
makeIso ''IndexDocument
makeIso ''LifecycleConfiguration
makeIso ''NoncurrentVersionExpiration
makeIso ''NotificationConfiguration
makeIso ''RequestPaymentConfiguration
makeIso ''RestoreRequest
makeIso ''Tagging

makeLenses ''AccessControlPolicy
makeLenses ''Bucket
makeLenses ''CORSRule
makeLenses ''CompletedPart
makeLenses ''Condition
makeLenses ''CopyObjectResult
makeLenses ''CopyPartResult
makeLenses ''Delete
makeLenses ''DeleteMarkerEntry
makeLenses ''DeletedObject
makeLenses ''Error
makeLenses ''Grant
makeLenses ''Grantee
makeLenses ''Initiator
makeLenses ''LifecycleExpiration
makeLenses ''LoggingEnabled
makeLenses ''MultipartUpload
makeLenses ''NoncurrentVersionTransition
makeLenses ''Object
makeLenses ''ObjectIdentifier
makeLenses ''ObjectVersion
makeLenses ''Owner
makeLenses ''Part
makeLenses ''Redirect
makeLenses ''RedirectAllRequestsTo
makeLenses ''RoutingRule
makeLenses ''Rule
makeLenses ''Tag
makeLenses ''TargetGrant
makeLenses ''TopicConfiguration
makeLenses ''Transition
makeLenses ''VersioningConfiguration
makeLenses ''WebsiteConfiguration
