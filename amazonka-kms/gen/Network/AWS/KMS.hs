-- Module      : Network.AWS.KMS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Key Management Service (KMS) is a managed service that makes it easy
-- for you to create and control the encryption keys used to encrypt your data,
-- and uses Hardware Security Modules (HSMs) to protect the security of your
-- keys. Amazon Key Management Service is integrated with other Amazon services
-- including Amazon EBS, Amazon S3, and Amazon Redshift. Amazon Key Management
-- Service is also integrated with Amazon CloudTrail to provide you with logs of
-- all key usage to help meet your regulatory and compliance needs.
module Network.AWS.KMS
    ( module Network.AWS.KMS.CreateAlias
    , module Network.AWS.KMS.CreateGrant
    , module Network.AWS.KMS.CreateKey
    , module Network.AWS.KMS.Decrypt
    , module Network.AWS.KMS.DeleteAlias
    , module Network.AWS.KMS.DescribeKey
    , module Network.AWS.KMS.DisableKey
    , module Network.AWS.KMS.DisableKeyRotation
    , module Network.AWS.KMS.EnableKey
    , module Network.AWS.KMS.EnableKeyRotation
    , module Network.AWS.KMS.Encrypt
    , module Network.AWS.KMS.GenerateDataKey
    , module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
    , module Network.AWS.KMS.GenerateRandom
    , module Network.AWS.KMS.GetKeyPolicy
    , module Network.AWS.KMS.GetKeyRotationStatus
    , module Network.AWS.KMS.ListAliases
    , module Network.AWS.KMS.ListGrants
    , module Network.AWS.KMS.ListKeyPolicies
    , module Network.AWS.KMS.ListKeys
    , module Network.AWS.KMS.PutKeyPolicy
    , module Network.AWS.KMS.ReEncrypt
    , module Network.AWS.KMS.RetireGrant
    , module Network.AWS.KMS.RevokeGrant
    , module Network.AWS.KMS.Types
    , module Network.AWS.KMS.UpdateKeyDescription
    ) where

import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.DeleteAlias
import Network.AWS.KMS.DescribeKey
import Network.AWS.KMS.DisableKey
import Network.AWS.KMS.DisableKeyRotation
import Network.AWS.KMS.EnableKey
import Network.AWS.KMS.EnableKeyRotation
import Network.AWS.KMS.Encrypt
import Network.AWS.KMS.GenerateDataKey
import Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
import Network.AWS.KMS.GenerateRandom
import Network.AWS.KMS.GetKeyPolicy
import Network.AWS.KMS.GetKeyRotationStatus
import Network.AWS.KMS.ListAliases
import Network.AWS.KMS.ListGrants
import Network.AWS.KMS.ListKeyPolicies
import Network.AWS.KMS.ListKeys
import Network.AWS.KMS.PutKeyPolicy
import Network.AWS.KMS.ReEncrypt
import Network.AWS.KMS.RetireGrant
import Network.AWS.KMS.RevokeGrant
import Network.AWS.KMS.Types
import Network.AWS.KMS.UpdateKeyDescription
