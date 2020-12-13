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
    mdpgDBParameterGroupName,
    mdpgParameters,

    -- * Destructuring the response
    DBParameterGroupNameMessage (..),
    mkDBParameterGroupNameMessage,

    -- ** Response lenses
    dpgnmDBParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyDBParameterGroup' smart constructor.
data ModifyDBParameterGroup = ModifyDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    --     * If supplied, must match the name of an existing @DBParameterGroup@ .
    dbParameterGroupName :: Lude.Text,
    -- | An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; later arguments are optional. A maximum of 20 parameters can be modified in a single request.
    --
    -- Valid Values (for the application method): @immediate | pending-reboot@
    parameters :: [Parameter]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing @DBParameterGroup@ .
--
--
-- * 'parameters' - An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; later arguments are optional. A maximum of 20 parameters can be modified in a single request.
--
-- Valid Values (for the application method): @immediate | pending-reboot@
mkModifyDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Lude.Text ->
  ModifyDBParameterGroup
mkModifyDBParameterGroup pDBParameterGroupName_ =
  ModifyDBParameterGroup'
    { dbParameterGroupName =
        pDBParameterGroupName_,
      parameters = Lude.mempty
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing @DBParameterGroup@ .
--
--
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpgDBParameterGroupName :: Lens.Lens' ModifyDBParameterGroup Lude.Text
mdpgDBParameterGroupName = Lens.lens (dbParameterGroupName :: ModifyDBParameterGroup -> Lude.Text) (\s a -> s {dbParameterGroupName = a} :: ModifyDBParameterGroup)
{-# DEPRECATED mdpgDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; later arguments are optional. A maximum of 20 parameters can be modified in a single request.
--
-- Valid Values (for the application method): @immediate | pending-reboot@
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpgParameters :: Lens.Lens' ModifyDBParameterGroup [Parameter]
mdpgParameters = Lens.lens (parameters :: ModifyDBParameterGroup -> [Parameter]) (\s a -> s {parameters = a} :: ModifyDBParameterGroup)
{-# DEPRECATED mdpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.AWSRequest ModifyDBParameterGroup where
  type Rs ModifyDBParameterGroup = DBParameterGroupNameMessage
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyDBParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBParameterGroup where
  toQuery ModifyDBParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "Parameters" Lude.=: Lude.toQueryList "Parameter" parameters
      ]
