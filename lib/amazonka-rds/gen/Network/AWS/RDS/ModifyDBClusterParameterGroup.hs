{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group. To modify more than one parameter, submit a list of the following: @ParameterName@ , @ParameterValue@ , and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
-- /Important:/ After you create a DB cluster parameter group, you should wait at least 5 minutes before creating your first DB cluster that uses that DB cluster parameter group as the default parameter group. This allows Amazon RDS to fully complete the create action before the parameter group is used as the default for a new DB cluster. This is especially important for parameters that are critical when creating the default database for a DB cluster, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the @DescribeDBClusterParameters@ action to verify that your DB cluster parameter group has been created or modified.
-- If the modified DB cluster parameter group is used by an Aurora Serverless cluster, Aurora applies the update immediately. The cluster restart might interrupt your workload. In that case, your application must reopen any connections and retry any transactions that were active when the parameter changes took effect.
module Network.AWS.RDS.ModifyDBClusterParameterGroup
  ( -- * Creating a request
    ModifyDBClusterParameterGroup (..),
    mkModifyDBClusterParameterGroup,

    -- ** Request lenses
    mdcpgDBClusterParameterGroupName,
    mdcpgParameters,

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
-- /See:/ 'mkModifyDBClusterParameterGroup' smart constructor.
data ModifyDBClusterParameterGroup = ModifyDBClusterParameterGroup'
  { dbClusterParameterGroupName ::
      Lude.Text,
    parameters :: [Parameter]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group to modify.
-- * 'parameters' - A list of parameters in the DB cluster parameter group to modify.
mkModifyDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Lude.Text ->
  ModifyDBClusterParameterGroup
mkModifyDBClusterParameterGroup pDBClusterParameterGroupName_ =
  ModifyDBClusterParameterGroup'
    { dbClusterParameterGroupName =
        pDBClusterParameterGroupName_,
      parameters = Lude.mempty
    }

-- | The name of the DB cluster parameter group to modify.
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcpgDBClusterParameterGroupName :: Lens.Lens' ModifyDBClusterParameterGroup Lude.Text
mdcpgDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: ModifyDBClusterParameterGroup -> Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: ModifyDBClusterParameterGroup)
{-# DEPRECATED mdcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | A list of parameters in the DB cluster parameter group to modify.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcpgParameters :: Lens.Lens' ModifyDBClusterParameterGroup [Parameter]
mdcpgParameters = Lens.lens (parameters :: ModifyDBClusterParameterGroup -> [Parameter]) (\s a -> s {parameters = a} :: ModifyDBClusterParameterGroup)
{-# DEPRECATED mdcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.AWSRequest ModifyDBClusterParameterGroup where
  type
    Rs ModifyDBClusterParameterGroup =
      DBClusterParameterGroupNameMessage
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBClusterParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyDBClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBClusterParameterGroup where
  toQuery ModifyDBClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyDBClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName,
        "Parameters" Lude.=: Lude.toQueryList "Parameter" parameters
      ]
