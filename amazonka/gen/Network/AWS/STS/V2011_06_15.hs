{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.STS.V2011_06_15
    ( module Network.AWS.STS.V2011_06_15.AssumeRole
    , module Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML
    , module Network.AWS.STS.V2011_06_15.AssumeRoleWithWebIdentity
    , module Network.AWS.STS.V2011_06_15.DecodeAuthorizationMessage
    , module Network.AWS.STS.V2011_06_15.GetFederationToken
    , module Network.AWS.STS.V2011_06_15.GetSessionToken
    , module Network.AWS.STS.V2011_06_15.Lenses
    , module Network.AWS.STS.V2011_06_15.Types
    ) where

import Network.AWS.STS.V2011_06_15.AssumeRole
import Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML
import Network.AWS.STS.V2011_06_15.AssumeRoleWithWebIdentity
import Network.AWS.STS.V2011_06_15.DecodeAuthorizationMessage
import Network.AWS.STS.V2011_06_15.GetFederationToken
import Network.AWS.STS.V2011_06_15.GetSessionToken
import Network.AWS.STS.V2011_06_15.Lenses
import Network.AWS.STS.V2011_06_15.Types
