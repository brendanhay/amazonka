{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group to the engine or system default value. You can reset specific parameters by submitting a list of parameter names. To reset the entire cache parameter group, specify the @ResetAllParameters@ and @CacheParameterGroupName@ parameters.
module Network.AWS.ElastiCache.ResetCacheParameterGroup
  ( -- * Creating a request
    ResetCacheParameterGroup (..),
    mkResetCacheParameterGroup,

    -- ** Request lenses
    rcpgCacheParameterGroupName,
    rcpgResetAllParameters,
    rcpgParameterNameValues,

    -- * Destructuring the response
    CacheParameterGroupNameMessage (..),
    mkCacheParameterGroupNameMessage,

    -- ** Response lenses
    cpgnmCacheParameterGroupName,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ResetCacheParameterGroup@ operation.
--
-- /See:/ 'mkResetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
  { -- | The name of the cache parameter group to reset.
    cacheParameterGroupName :: Lude.Text,
    -- | If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
    --
    -- Valid values: @true@ | @false@
    resetAllParameters :: Lude.Maybe Lude.Bool,
    -- | An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
    parameterNameValues :: Lude.Maybe [ParameterNameValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetCacheParameterGroup' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupName' - The name of the cache parameter group to reset.
-- * 'resetAllParameters' - If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
-- * 'parameterNameValues' - An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
mkResetCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Lude.Text ->
  ResetCacheParameterGroup
mkResetCacheParameterGroup pCacheParameterGroupName_ =
  ResetCacheParameterGroup'
    { cacheParameterGroupName =
        pCacheParameterGroupName_,
      resetAllParameters = Lude.Nothing,
      parameterNameValues = Lude.Nothing
    }

-- | The name of the cache parameter group to reset.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgCacheParameterGroupName :: Lens.Lens' ResetCacheParameterGroup Lude.Text
rcpgCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: ResetCacheParameterGroup -> Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: ResetCacheParameterGroup)
{-# DEPRECATED rcpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | If @true@ , all parameters in the cache parameter group are reset to their default values. If @false@ , only the parameters listed by @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgResetAllParameters :: Lens.Lens' ResetCacheParameterGroup (Lude.Maybe Lude.Bool)
rcpgResetAllParameters = Lens.lens (resetAllParameters :: ResetCacheParameterGroup -> Lude.Maybe Lude.Bool) (\s a -> s {resetAllParameters = a} :: ResetCacheParameterGroup)
{-# DEPRECATED rcpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

-- | An array of parameter names to reset to their default values. If @ResetAllParameters@ is @true@ , do not use @ParameterNameValues@ . If @ResetAllParameters@ is @false@ , you must specify the name of at least one parameter to reset.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpgParameterNameValues :: Lens.Lens' ResetCacheParameterGroup (Lude.Maybe [ParameterNameValue])
rcpgParameterNameValues = Lens.lens (parameterNameValues :: ResetCacheParameterGroup -> Lude.Maybe [ParameterNameValue]) (\s a -> s {parameterNameValues = a} :: ResetCacheParameterGroup)
{-# DEPRECATED rcpgParameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead." #-}

instance Lude.AWSRequest ResetCacheParameterGroup where
  type Rs ResetCacheParameterGroup = CacheParameterGroupNameMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ResetCacheParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ResetCacheParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetCacheParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetCacheParameterGroup where
  toQuery ResetCacheParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ResetCacheParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "ResetAllParameters" Lude.=: resetAllParameters,
        "ParameterNameValues"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "ParameterNameValue"
                Lude.<$> parameterNameValues
            )
      ]
