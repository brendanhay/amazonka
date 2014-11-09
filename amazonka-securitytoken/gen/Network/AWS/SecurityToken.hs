-- Module      : Network.AWS.SecurityToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AWS Security Token Service (STS) is a web service that enables you to
-- request temporary, limited-privilege credentials for AWS Identity and
-- Access Management (IAM) users or for users that you authenticate (federated
-- users).
module Network.AWS.SecurityToken
    ( module Network.AWS.SecurityToken.AssumeRole
    , module Network.AWS.SecurityToken.AssumeRoleWithSAML
    , module Network.AWS.SecurityToken.AssumeRoleWithWebIdentity
    , module Network.AWS.SecurityToken.DecodeAuthorizationMessage
    , module Network.AWS.SecurityToken.GetFederationToken
    , module Network.AWS.SecurityToken.GetSessionToken
    , module Network.AWS.SecurityToken.Types
    ) where

import Network.AWS.SecurityToken.AssumeRole
import Network.AWS.SecurityToken.AssumeRoleWithSAML
import Network.AWS.SecurityToken.AssumeRoleWithWebIdentity
import Network.AWS.SecurityToken.DecodeAuthorizationMessage
import Network.AWS.SecurityToken.GetFederationToken
import Network.AWS.SecurityToken.GetSessionToken
import Network.AWS.SecurityToken.Types
