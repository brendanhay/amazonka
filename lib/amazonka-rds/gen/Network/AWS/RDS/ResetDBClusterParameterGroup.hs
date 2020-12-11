{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ResetDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group to the default value. To reset specific parameters submit a list of the following: @ParameterName@ and @ApplyMethod@ . To reset the entire DB cluster parameter group, specify the @DBClusterParameterGroupName@ and @ResetAllParameters@ parameters.
--
-- When resetting the entire group, dynamic parameters are updated immediately and static parameters are set to @pending-reboot@ to take effect on the next DB instance restart or @RebootDBInstance@ request. You must call @RebootDBInstance@ for every DB instance in your DB cluster that you want the updated static parameter to apply to.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.ResetDBClusterParameterGroup
  ( -- * Creating a request
    ResetDBClusterParameterGroup (..),
    mkResetDBClusterParameterGroup,

    -- ** Request lenses
    rdcpgResetAllParameters,
    rdcpgParameters,
    rdcpgDBClusterParameterGroupName,

    -- * Destructuring the response
    DBClusterParameterGroupNameMessage (..),
    mkDBClusterParameterGroupNameMessage,

    -- ** Response lenses
    dcpgnmDBClusterParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkResetDBClusterParameterGroup' smart constructor.
data ResetDBClusterParameterGroup = ResetDBClusterParameterGroup'
  { resetAllParameters ::
      Lude.Maybe Lude.Bool,
    parameters ::
      Lude.Maybe [Parameter],
    dbClusterParameterGroupName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to reset.
-- * 'parameters' - A list of parameter names in the DB cluster parameter group to reset to the default values. You can't use this parameter if the @ResetAllParameters@ parameter is enabled.
-- * 'resetAllParameters' - A value that indicates whether to reset all parameters in the DB cluster parameter group to their default values. You can't use this parameter if there is a list of parameter names specified for the @Parameters@ parameter.
mkResetDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Lude.Text ->
  ResetDBClusterParameterGroup
mkResetDBClusterParameterGroup pDBClusterParameterGroupName_ =
  ResetDBClusterParameterGroup'
    { resetAllParameters = Lude.Nothing,
      parameters = Lude.Nothing,
      dbClusterParameterGroupName = pDBClusterParameterGroupName_
    }

-- | A value that indicates whether to reset all parameters in the DB cluster parameter group to their default values. You can't use this parameter if there is a list of parameter names specified for the @Parameters@ parameter.
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcpgResetAllParameters :: Lens.Lens' ResetDBClusterParameterGroup (Lude.Maybe Lude.Bool)
rdcpgResetAllParameters = Lens.lens (resetAllParameters :: ResetDBClusterParameterGroup -> Lude.Maybe Lude.Bool) (\s a -> s {resetAllParameters = a} :: ResetDBClusterParameterGroup)
{-# DEPRECATED rdcpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

-- | A list of parameter names in the DB cluster parameter group to reset to the default values. You can't use this parameter if the @ResetAllParameters@ parameter is enabled.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcpgParameters :: Lens.Lens' ResetDBClusterParameterGroup (Lude.Maybe [Parameter])
rdcpgParameters = Lens.lens (parameters :: ResetDBClusterParameterGroup -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: ResetDBClusterParameterGroup)
{-# DEPRECATED rdcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the DB cluster parameter group to reset.
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcpgDBClusterParameterGroupName :: Lens.Lens' ResetDBClusterParameterGroup Lude.Text
rdcpgDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: ResetDBClusterParameterGroup -> Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: ResetDBClusterParameterGroup)
{-# DEPRECATED rdcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

instance Lude.AWSRequest ResetDBClusterParameterGroup where
  type
    Rs ResetDBClusterParameterGroup =
      DBClusterParameterGroupNameMessage
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ResetDBClusterParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ResetDBClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetDBClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetDBClusterParameterGroup where
  toQuery ResetDBClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ResetDBClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ResetAllParameters" Lude.=: resetAllParameters,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Parameter" Lude.<$> parameters),
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName
      ]
