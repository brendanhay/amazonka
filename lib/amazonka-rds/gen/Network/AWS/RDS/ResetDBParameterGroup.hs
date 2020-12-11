{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB parameter group to the engine/system default value. To reset specific parameters, provide a list of the following: @ParameterName@ and @ApplyMethod@ . To reset the entire DB parameter group, specify the @DBParameterGroup@ name and @ResetAllParameters@ parameters. When resetting the entire group, dynamic parameters are updated immediately and static parameters are set to @pending-reboot@ to take effect on the next DB instance restart or @RebootDBInstance@ request.
module Network.AWS.RDS.ResetDBParameterGroup
  ( -- * Creating a request
    ResetDBParameterGroup (..),
    mkResetDBParameterGroup,

    -- ** Request lenses
    rdpgResetAllParameters,
    rdpgParameters,
    rdpgDBParameterGroupName,

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
-- /See:/ 'mkResetDBParameterGroup' smart constructor.
data ResetDBParameterGroup = ResetDBParameterGroup'
  { resetAllParameters ::
      Lude.Maybe Lude.Bool,
    parameters :: Lude.Maybe [Parameter],
    dbParameterGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetDBParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing @DBParameterGroup@ .
--
--
-- * 'parameters' - To reset the entire DB parameter group, specify the @DBParameterGroup@ name and @ResetAllParameters@ parameters. To reset specific parameters, provide a list of the following: @ParameterName@ and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
-- __MySQL__
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
-- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
-- __MariaDB__
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
-- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
-- __Oracle__
-- Valid Values (for Apply method): @pending-reboot@
-- * 'resetAllParameters' - A value that indicates whether to reset all parameters in the DB parameter group to default values. By default, all parameters in the DB parameter group are reset to default values.
mkResetDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Lude.Text ->
  ResetDBParameterGroup
mkResetDBParameterGroup pDBParameterGroupName_ =
  ResetDBParameterGroup'
    { resetAllParameters = Lude.Nothing,
      parameters = Lude.Nothing,
      dbParameterGroupName = pDBParameterGroupName_
    }

-- | A value that indicates whether to reset all parameters in the DB parameter group to default values. By default, all parameters in the DB parameter group are reset to default values.
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpgResetAllParameters :: Lens.Lens' ResetDBParameterGroup (Lude.Maybe Lude.Bool)
rdpgResetAllParameters = Lens.lens (resetAllParameters :: ResetDBParameterGroup -> Lude.Maybe Lude.Bool) (\s a -> s {resetAllParameters = a} :: ResetDBParameterGroup)
{-# DEPRECATED rdpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

-- | To reset the entire DB parameter group, specify the @DBParameterGroup@ name and @ResetAllParameters@ parameters. To reset specific parameters, provide a list of the following: @ParameterName@ and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
-- __MySQL__
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
-- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
-- __MariaDB__
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
-- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
-- __Oracle__
-- Valid Values (for Apply method): @pending-reboot@
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpgParameters :: Lens.Lens' ResetDBParameterGroup (Lude.Maybe [Parameter])
rdpgParameters = Lens.lens (parameters :: ResetDBParameterGroup -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: ResetDBParameterGroup)
{-# DEPRECATED rdpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing @DBParameterGroup@ .
--
--
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpgDBParameterGroupName :: Lens.Lens' ResetDBParameterGroup Lude.Text
rdpgDBParameterGroupName = Lens.lens (dbParameterGroupName :: ResetDBParameterGroup -> Lude.Text) (\s a -> s {dbParameterGroupName = a} :: ResetDBParameterGroup)
{-# DEPRECATED rdpgDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

instance Lude.AWSRequest ResetDBParameterGroup where
  type Rs ResetDBParameterGroup = DBParameterGroupNameMessage
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ResetDBParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ResetDBParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetDBParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetDBParameterGroup where
  toQuery ResetDBParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResetDBParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ResetAllParameters" Lude.=: resetAllParameters,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Parameter" Lude.<$> parameters),
        "DBParameterGroupName" Lude.=: dbParameterGroupName
      ]
