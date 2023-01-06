-- |
-- Module      : Amazonka.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Re-export lenses and other optics for types in @amazonka@ and
-- @amazonka-core@. You will probably find record updates,
-- [generic-lens](https://hackage.haskell.org/package/generic-lens),
-- [generic-optics](https://hackage.haskell.org/package/generic-optics),
-- or (GHC >=9.2) @-XOverloadedRecordDot@ more ergonomic than these.
module Amazonka.Lens
  ( -- * Amazonka.Auth.SSO

    -- ** CachedAccessToken
    cachedAccessToken_startUrl,
    cachedAccessToken_region,
    cachedAccessToken_accessToken,
    cachedAccessToken_expiresAt,

    -- * Amazonka.EC2.Metadata

    -- ** IdentityDocument
    identityDocument_devpayProductCodes,
    identityDocument_billingProducts,
    identityDocument_version,
    identityDocument_privateIp,
    identityDocument_availabilityZone,
    identityDocument_region,
    identityDocument_instanceId,
    identityDocument_instanceType,
    identityDocument_accountId,
    identityDocument_imageId,
    identityDocument_kernelId,
    identityDocument_ramdiskId,
    identityDocument_architecture,
    identityDocument_pendingTime,

    -- * Amazonka.Env

    -- ** Env'
    env_region,
    env_logger,
    env_hooks,
    env_retryCheck,
    env_overrides,
    env_manager,
    env_auth,

    -- * Amazonka.Core.Lens
    module Amazonka.Core.Lens,
  )
where

import Amazonka.Auth.SSO
import Amazonka.Core.Lens
import Amazonka.EC2.Metadata
import Amazonka.Env
