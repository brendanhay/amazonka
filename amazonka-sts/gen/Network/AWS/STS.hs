-- Module      : Network.AWS.STS
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

-- | The AWS Security Token Service (STS) is a web service that enables you to
-- request temporary, limited-privilege credentials for AWS Identity and Access
-- Management (IAM) users or for users that you authenticate (federated users).
module Network.AWS.STS
    ( module Network.AWS.STS.AssumeRole
    , module Network.AWS.STS.AssumeRoleWithSAML
    , module Network.AWS.STS.AssumeRoleWithWebIdentity
    , module Network.AWS.STS.DecodeAuthorizationMessage
    , module Network.AWS.STS.GetFederationToken
    , module Network.AWS.STS.GetSessionToken
    , module Network.AWS.STS.Types
    ) where

import Network.AWS.STS.AssumeRole
import Network.AWS.STS.AssumeRoleWithSAML
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
import Network.AWS.STS.Types
