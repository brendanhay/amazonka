{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB parameter group. To modify more than one parameter, submit a list of the following: @ParameterName@ , @ParameterValue@ , and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
-- /Important:/ After you modify a DB parameter group, you should wait at least 5 minutes before creating your first DB instance that uses that DB parameter group as the default parameter group. This allows Amazon RDS to fully complete the modify action before the parameter group is used as the default for a new DB instance. This is especially important for parameters that are critical when creating the default database for a DB instance, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the /DescribeDBParameters/ command to verify that your DB parameter group has been created or modified.
module Network.AWS.RDS.ModifyDBParameterGroup
  ( -- * Creating a request
    ModifyDBParameterGroup (..),
    mkModifyDBParameterGroup,

    -- ** Request lenses
    mdbpgDBParameterGroupName,
    mdbpgParameters,

    -- * Destructuring the response
    Types.DBParameterGroupNameMessage (..),
    Types.mkDBParameterGroupNameMessage,

    -- ** Response lenses
    Types.dbpgnmDBParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyDBParameterGroup' smart constructor.
data ModifyDBParameterGroup = ModifyDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing @DBParameterGroup@ .
    dBParameterGroupName :: Types.String,
    -- | An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; later arguments are optional. A maximum of 20 parameters can be modified in a single request.
    --
    -- Valid Values (for the application method): @immediate | pending-reboot@
    parameters :: [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBParameterGroup' value with any optional fields omitted.
mkModifyDBParameterGroup ::
  -- | 'dBParameterGroupName'
  Types.String ->
  ModifyDBParameterGroup
mkModifyDBParameterGroup dBParameterGroupName =
  ModifyDBParameterGroup'
    { dBParameterGroupName,
      parameters = Core.mempty
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing @DBParameterGroup@ .
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpgDBParameterGroupName :: Lens.Lens' ModifyDBParameterGroup Types.String
mdbpgDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED mdbpgDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; later arguments are optional. A maximum of 20 parameters can be modified in a single request.
--
-- Valid Values (for the application method): @immediate | pending-reboot@
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbpgParameters :: Lens.Lens' ModifyDBParameterGroup [Types.Parameter]
mdbpgParameters = Lens.field @"parameters"
{-# DEPRECATED mdbpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.AWSRequest ModifyDBParameterGroup where
  type Rs ModifyDBParameterGroup = Types.DBParameterGroupNameMessage
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
            ( Core.pure ("Action", "ModifyDBParameterGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBParameterGroupName" dBParameterGroupName)
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "Parameter" parameters)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBParameterGroupResult"
      (\s h x -> Core.parseXML x)
