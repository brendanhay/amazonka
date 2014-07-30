{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.V2011_06_15.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.STS.V2011_06_15.Lenses where

import Control.Lens.TH
import Network.AWS.STS.V2011_06_15.Types
import Network.AWS.STS.V2011_06_15.AssumeRole
import Network.AWS.STS.V2011_06_15.DecodeAuthorizationMessage
import Network.AWS.STS.V2011_06_15.AssumeRoleWithWebIdentity
import Network.AWS.STS.V2011_06_15.GetFederationToken
import Network.AWS.STS.V2011_06_15.GetSessionToken
import Network.AWS.STS.V2011_06_15.AssumeRoleWithSAML

-- Newtypes

-- Products
makeLenses ''AssumedRoleUser
makeLenses ''Credentials
makeLenses ''FederatedUser

-- Requests
makeLenses ''AssumeRole
makeLenses ''DecodeAuthorizationMessage
makeLenses ''AssumeRoleWithWebIdentity
makeLenses ''GetFederationToken
makeLenses ''GetSessionToken
makeLenses ''AssumeRoleWithSAML

-- Responses
makeLenses ''AssumeRoleResponse
makeLenses ''DecodeAuthorizationMessageResponse
makeLenses ''AssumeRoleWithWebIdentityResponse
makeLenses ''GetFederationTokenResponse
makeLenses ''GetSessionTokenResponse
makeLenses ''AssumeRoleWithSAMLResponse
