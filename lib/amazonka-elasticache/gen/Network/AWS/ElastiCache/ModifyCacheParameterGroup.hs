{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
  ( -- * Creating a request
    ModifyCacheParameterGroup (..),
    mkModifyCacheParameterGroup,

    -- ** Request lenses
    mcpgCacheParameterGroupName,
    mcpgParameterNameValues,

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

-- | Represents the input of a @ModifyCacheParameterGroup@ operation.
--
-- /See:/ 'mkModifyCacheParameterGroup' smart constructor.
data ModifyCacheParameterGroup = ModifyCacheParameterGroup'
  { -- | The name of the cache parameter group to modify.
    cacheParameterGroupName :: Lude.Text,
    -- | An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
    parameterNameValues :: [ParameterNameValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCacheParameterGroup' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupName' - The name of the cache parameter group to modify.
-- * 'parameterNameValues' - An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
mkModifyCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Lude.Text ->
  ModifyCacheParameterGroup
mkModifyCacheParameterGroup pCacheParameterGroupName_ =
  ModifyCacheParameterGroup'
    { cacheParameterGroupName =
        pCacheParameterGroupName_,
      parameterNameValues = Lude.mempty
    }

-- | The name of the cache parameter group to modify.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgCacheParameterGroupName :: Lens.Lens' ModifyCacheParameterGroup Lude.Text
mcpgCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: ModifyCacheParameterGroup -> Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: ModifyCacheParameterGroup)
{-# DEPRECATED mcpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | An array of parameter names and values for the parameter update. You must supply at least one parameter name and value; subsequent arguments are optional. A maximum of 20 parameters may be modified per request.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameterNameValues :: Lens.Lens' ModifyCacheParameterGroup [ParameterNameValue]
mcpgParameterNameValues = Lens.lens (parameterNameValues :: ModifyCacheParameterGroup -> [ParameterNameValue]) (\s a -> s {parameterNameValues = a} :: ModifyCacheParameterGroup)
{-# DEPRECATED mcpgParameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead." #-}

instance Lude.AWSRequest ModifyCacheParameterGroup where
  type Rs ModifyCacheParameterGroup = CacheParameterGroupNameMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyCacheParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyCacheParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCacheParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCacheParameterGroup where
  toQuery ModifyCacheParameterGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyCacheParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "ParameterNameValues"
          Lude.=: Lude.toQueryList "ParameterNameValue" parameterNameValues
      ]
