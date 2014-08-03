-- Module      : Network.AWS.STS.V2011_06_15
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
module Network.AWS.STS.V2011_06_15 (module Export) where

import Network.AWS.STS.V2011_06_15.AssumeRole as Export
import Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML as Export
import Network.AWS.STS.V2011_06_15.AssumeRoleWithWebIdentity as Export
import Network.AWS.STS.V2011_06_15.DecodeAuthorizationMessage as Export
import Network.AWS.STS.V2011_06_15.GetFederationToken as Export
import Network.AWS.STS.V2011_06_15.GetSessionToken as Export
import Network.AWS.STS.V2011_06_15.Types as Export
