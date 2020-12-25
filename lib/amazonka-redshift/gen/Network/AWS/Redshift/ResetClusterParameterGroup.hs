{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets one or more parameters of the specified parameter group to their default values and sets the source values of the parameters to "engine-default". To reset the entire parameter group specify the /ResetAllParameters/ parameter. For parameter changes to take effect you must reboot any associated clusters.
module Network.AWS.Redshift.ResetClusterParameterGroup
  ( -- * Creating a request
    ResetClusterParameterGroup (..),
    mkResetClusterParameterGroup,

    -- ** Request lenses
    rcpgParameterGroupName,
    rcpgParameters,
    rcpgResetAllParameters,

    -- * Destructuring the response
    Types.ClusterParameterGroupNameMessage (..),
    Types.mkClusterParameterGroupNameMessage,

    -- ** Response lenses
    Types.cpgnmParameterGroupName,
    Types.cpgnmParameterGroupStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkResetClusterParameterGroup' smart constructor.
data ResetClusterParameterGroup = ResetClusterParameterGroup'
  { -- | The name of the cluster parameter group to be reset.
    parameterGroupName :: Types.ParameterGroupName,
    -- | An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.
    --
    -- Constraints: A maximum of 20 parameters can be reset in a single request.
    parameters :: Core.Maybe [Types.Parameter],
    -- | If @true@ , all parameters in the specified parameter group will be reset to their default values.
    --
    -- Default: @true@
    resetAllParameters :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetClusterParameterGroup' value with any optional fields omitted.
mkResetClusterParameterGroup ::
  -- | 'parameterGroupName'
  Types.ParameterGroupName ->
  ResetClusterParameterGroup
mkResetClusterParameterGroup parameterGroupName =
  ResetClusterParameterGroup'
    { parameterGroupName,
      parameters = Core.Nothing,
      resetAllParameters = Core.Nothing
    }

-- | The name of the cluster parameter group to be reset.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameterGroupName :: Lens.Lens' ResetClusterParameterGroup Types.ParameterGroupName
rcpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED rcpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameters :: Lens.Lens' ResetClusterParameterGroup (Core.Maybe [Types.Parameter])
rcpgParameters = Lens.field @"parameters"
{-# DEPRECATED rcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | If @true@ , all parameters in the specified parameter group will be reset to their default values.
--
-- Default: @true@
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgResetAllParameters :: Lens.Lens' ResetClusterParameterGroup (Core.Maybe Core.Bool)
rcpgResetAllParameters = Lens.field @"resetAllParameters"
{-# DEPRECATED rcpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

instance Core.AWSRequest ResetClusterParameterGroup where
  type
    Rs ResetClusterParameterGroup =
      Types.ClusterParameterGroupNameMessage
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
            ( Core.pure ("Action", "ResetClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ParameterGroupName" parameterGroupName)
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "Parameter" Core.<$> parameters)
                        )
                Core.<> ( Core.toQueryValue "ResetAllParameters"
                            Core.<$> resetAllParameters
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ResetClusterParameterGroupResult"
      (\s h x -> Core.parseXML x)
