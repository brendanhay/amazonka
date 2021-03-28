{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ResetClusterParameterGroup (..)
    , mkResetClusterParameterGroup
    -- ** Request lenses
    , rcpgParameterGroupName
    , rcpgParameters
    , rcpgResetAllParameters

     -- * Destructuring the response
    , Types.ClusterParameterGroupNameMessage (..)
    , Types.mkClusterParameterGroupNameMessage
    -- ** Response lenses
    , Types.cpgnmParameterGroupName
    , Types.cpgnmParameterGroupStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkResetClusterParameterGroup' smart constructor.
data ResetClusterParameterGroup = ResetClusterParameterGroup'
  { parameterGroupName :: Core.Text
    -- ^ The name of the cluster parameter group to be reset.
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied. 
--
-- Constraints: A maximum of 20 parameters can be reset in a single request.
  , resetAllParameters :: Core.Maybe Core.Bool
    -- ^ If @true@ , all parameters in the specified parameter group will be reset to their default values. 
--
-- Default: @true@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetClusterParameterGroup' value with any optional fields omitted.
mkResetClusterParameterGroup
    :: Core.Text -- ^ 'parameterGroupName'
    -> ResetClusterParameterGroup
mkResetClusterParameterGroup parameterGroupName
  = ResetClusterParameterGroup'{parameterGroupName,
                                parameters = Core.Nothing, resetAllParameters = Core.Nothing}

-- | The name of the cluster parameter group to be reset.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameterGroupName :: Lens.Lens' ResetClusterParameterGroup Core.Text
rcpgParameterGroupName = Lens.field @"parameterGroupName"
{-# INLINEABLE rcpgParameterGroupName #-}
{-# DEPRECATED parameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead"  #-}

-- | An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied. 
--
-- Constraints: A maximum of 20 parameters can be reset in a single request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameters :: Lens.Lens' ResetClusterParameterGroup (Core.Maybe [Types.Parameter])
rcpgParameters = Lens.field @"parameters"
{-# INLINEABLE rcpgParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | If @true@ , all parameters in the specified parameter group will be reset to their default values. 
--
-- Default: @true@ 
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgResetAllParameters :: Lens.Lens' ResetClusterParameterGroup (Core.Maybe Core.Bool)
rcpgResetAllParameters = Lens.field @"resetAllParameters"
{-# INLINEABLE rcpgResetAllParameters #-}
{-# DEPRECATED resetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead"  #-}

instance Core.ToQuery ResetClusterParameterGroup where
        toQuery ResetClusterParameterGroup{..}
          = Core.toQueryPair "Action"
              ("ResetClusterParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ParameterGroupName" parameterGroupName
              Core.<>
              Core.toQueryPair "Parameters"
                (Core.maybe Core.mempty (Core.toQueryList "Parameter") parameters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResetAllParameters")
                resetAllParameters

instance Core.ToHeaders ResetClusterParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetClusterParameterGroup where
        type Rs ResetClusterParameterGroup =
             Types.ClusterParameterGroupNameMessage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ResetClusterParameterGroupResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
