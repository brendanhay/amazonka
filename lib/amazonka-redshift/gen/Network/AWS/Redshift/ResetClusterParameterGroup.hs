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
    rcpgResetAllParameters,
    rcpgParameters,
    rcpgParameterGroupName,

    -- * Destructuring the response
    ClusterParameterGroupNameMessage (..),
    mkClusterParameterGroupNameMessage,

    -- ** Response lenses
    cpgnmParameterGroupStatus,
    cpgnmParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkResetClusterParameterGroup' smart constructor.
data ResetClusterParameterGroup = ResetClusterParameterGroup'
  { -- | If @true@ , all parameters in the specified parameter group will be reset to their default values.
    --
    -- Default: @true@
    resetAllParameters :: Lude.Maybe Lude.Bool,
    -- | An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.
    --
    -- Constraints: A maximum of 20 parameters can be reset in a single request.
    parameters :: Lude.Maybe [Parameter],
    -- | The name of the cluster parameter group to be reset.
    parameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'resetAllParameters' - If @true@ , all parameters in the specified parameter group will be reset to their default values.
--
-- Default: @true@
-- * 'parameters' - An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single request.
-- * 'parameterGroupName' - The name of the cluster parameter group to be reset.
mkResetClusterParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  ResetClusterParameterGroup
mkResetClusterParameterGroup pParameterGroupName_ =
  ResetClusterParameterGroup'
    { resetAllParameters = Lude.Nothing,
      parameters = Lude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | If @true@ , all parameters in the specified parameter group will be reset to their default values.
--
-- Default: @true@
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgResetAllParameters :: Lens.Lens' ResetClusterParameterGroup (Lude.Maybe Lude.Bool)
rcpgResetAllParameters = Lens.lens (resetAllParameters :: ResetClusterParameterGroup -> Lude.Maybe Lude.Bool) (\s a -> s {resetAllParameters = a} :: ResetClusterParameterGroup)
{-# DEPRECATED rcpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

-- | An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameters :: Lens.Lens' ResetClusterParameterGroup (Lude.Maybe [Parameter])
rcpgParameters = Lens.lens (parameters :: ResetClusterParameterGroup -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: ResetClusterParameterGroup)
{-# DEPRECATED rcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the cluster parameter group to be reset.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameterGroupName :: Lens.Lens' ResetClusterParameterGroup Lude.Text
rcpgParameterGroupName = Lens.lens (parameterGroupName :: ResetClusterParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: ResetClusterParameterGroup)
{-# DEPRECATED rcpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.AWSRequest ResetClusterParameterGroup where
  type
    Rs ResetClusterParameterGroup =
      ClusterParameterGroupNameMessage
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ResetClusterParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ResetClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetClusterParameterGroup where
  toQuery ResetClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ResetClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ResetAllParameters" Lude.=: resetAllParameters,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Parameter" Lude.<$> parameters),
        "ParameterGroupName" Lude.=: parameterGroupName
      ]
