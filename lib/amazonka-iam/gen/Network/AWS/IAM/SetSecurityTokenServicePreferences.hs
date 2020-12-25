{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SetSecurityTokenServicePreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the global endpoint token as the token version used for the AWS account.
--
-- By default, AWS Security Token Service (STS) is available as a global service, and all STS requests go to a single endpoint at @https://sts.amazonaws.com@ . AWS recommends using Regional STS endpoints to reduce latency, build in redundancy, and increase session token availability. For information about Regional endpoints for STS, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#sts_region AWS Regions and Endpoints> in the /AWS General Reference/ .
-- If you make an STS call to the global endpoint, the resulting session tokens might be valid in some Regions but not others. It depends on the version that is set in this operation. Version 1 tokens are valid only in AWS Regions that are available by default. These tokens do not work in manually enabled Regions, such as Asia Pacific (Hong Kong). Version 2 tokens are valid in all Regions. However, version 2 tokens are longer and might affect systems where you temporarily store tokens. For information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating STS in an AWS Region> in the /IAM User Guide/ .
-- To view the current session token version, see the @GlobalEndpointTokenVersion@ entry in the response of the 'GetAccountSummary' operation.
module Network.AWS.IAM.SetSecurityTokenServicePreferences
  ( -- * Creating a request
    SetSecurityTokenServicePreferences (..),
    mkSetSecurityTokenServicePreferences,

    -- ** Request lenses
    sstspGlobalEndpointTokenVersion,

    -- * Destructuring the response
    SetSecurityTokenServicePreferencesResponse (..),
    mkSetSecurityTokenServicePreferencesResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetSecurityTokenServicePreferences' smart constructor.
newtype SetSecurityTokenServicePreferences = SetSecurityTokenServicePreferences'
  { -- | The version of the global endpoint token. Version 1 tokens are valid only in AWS Regions that are available by default. These tokens do not work in manually enabled Regions, such as Asia Pacific (Hong Kong). Version 2 tokens are valid in all Regions. However, version 2 tokens are longer and might affect systems where you temporarily store tokens.
    --
    -- For information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating STS in an AWS Region> in the /IAM User Guide/ .
    globalEndpointTokenVersion :: Types.GlobalEndpointTokenVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetSecurityTokenServicePreferences' value with any optional fields omitted.
mkSetSecurityTokenServicePreferences ::
  -- | 'globalEndpointTokenVersion'
  Types.GlobalEndpointTokenVersion ->
  SetSecurityTokenServicePreferences
mkSetSecurityTokenServicePreferences globalEndpointTokenVersion =
  SetSecurityTokenServicePreferences' {globalEndpointTokenVersion}

-- | The version of the global endpoint token. Version 1 tokens are valid only in AWS Regions that are available by default. These tokens do not work in manually enabled Regions, such as Asia Pacific (Hong Kong). Version 2 tokens are valid in all Regions. However, version 2 tokens are longer and might affect systems where you temporarily store tokens.
--
-- For information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating STS in an AWS Region> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'globalEndpointTokenVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstspGlobalEndpointTokenVersion :: Lens.Lens' SetSecurityTokenServicePreferences Types.GlobalEndpointTokenVersion
sstspGlobalEndpointTokenVersion = Lens.field @"globalEndpointTokenVersion"
{-# DEPRECATED sstspGlobalEndpointTokenVersion "Use generic-lens or generic-optics with 'globalEndpointTokenVersion' instead." #-}

instance Core.AWSRequest SetSecurityTokenServicePreferences where
  type
    Rs SetSecurityTokenServicePreferences =
      SetSecurityTokenServicePreferencesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SetSecurityTokenServicePreferences")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> ( Core.toQueryValue
                            "GlobalEndpointTokenVersion"
                            globalEndpointTokenVersion
                        )
            )
      }
  response =
    Response.receiveNull SetSecurityTokenServicePreferencesResponse'

-- | /See:/ 'mkSetSecurityTokenServicePreferencesResponse' smart constructor.
data SetSecurityTokenServicePreferencesResponse = SetSecurityTokenServicePreferencesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSecurityTokenServicePreferencesResponse' value with any optional fields omitted.
mkSetSecurityTokenServicePreferencesResponse ::
  SetSecurityTokenServicePreferencesResponse
mkSetSecurityTokenServicePreferencesResponse =
  SetSecurityTokenServicePreferencesResponse'
