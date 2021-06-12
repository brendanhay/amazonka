{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group to the engine or
-- system default value. You can reset specific parameters by submitting a
-- list of parameter names. To reset the entire cache parameter group,
-- specify the @ResetAllParameters@ and @CacheParameterGroupName@
-- parameters.
module Network.AWS.ElastiCache.ResetCacheParameterGroup
  ( -- * Creating a Request
    ResetCacheParameterGroup (..),
    newResetCacheParameterGroup,

    -- * Request Lenses
    resetCacheParameterGroup_resetAllParameters,
    resetCacheParameterGroup_parameterNameValues,
    resetCacheParameterGroup_cacheParameterGroupName,

    -- * Destructuring the Response
    CacheParameterGroupNameMessage (..),
    newCacheParameterGroupNameMessage,

    -- * Response Lenses
    cacheParameterGroupNameMessage_cacheParameterGroupName,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ResetCacheParameterGroup@ operation.
--
-- /See:/ 'newResetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
  { -- | If @true@, all parameters in the cache parameter group are reset to
    -- their default values. If @false@, only the parameters listed by
    -- @ParameterNameValues@ are reset to their default values.
    --
    -- Valid values: @true@ | @false@
    resetAllParameters :: Core.Maybe Core.Bool,
    -- | An array of parameter names to reset to their default values. If
    -- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
    -- @ResetAllParameters@ is @false@, you must specify the name of at least
    -- one parameter to reset.
    parameterNameValues :: Core.Maybe [ParameterNameValue],
    -- | The name of the cache parameter group to reset.
    cacheParameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetCacheParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resetAllParameters', 'resetCacheParameterGroup_resetAllParameters' - If @true@, all parameters in the cache parameter group are reset to
-- their default values. If @false@, only the parameters listed by
-- @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
--
-- 'parameterNameValues', 'resetCacheParameterGroup_parameterNameValues' - An array of parameter names to reset to their default values. If
-- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
-- @ResetAllParameters@ is @false@, you must specify the name of at least
-- one parameter to reset.
--
-- 'cacheParameterGroupName', 'resetCacheParameterGroup_cacheParameterGroupName' - The name of the cache parameter group to reset.
newResetCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Core.Text ->
  ResetCacheParameterGroup
newResetCacheParameterGroup pCacheParameterGroupName_ =
  ResetCacheParameterGroup'
    { resetAllParameters =
        Core.Nothing,
      parameterNameValues = Core.Nothing,
      cacheParameterGroupName =
        pCacheParameterGroupName_
    }

-- | If @true@, all parameters in the cache parameter group are reset to
-- their default values. If @false@, only the parameters listed by
-- @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
resetCacheParameterGroup_resetAllParameters :: Lens.Lens' ResetCacheParameterGroup (Core.Maybe Core.Bool)
resetCacheParameterGroup_resetAllParameters = Lens.lens (\ResetCacheParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetCacheParameterGroup' {} a -> s {resetAllParameters = a} :: ResetCacheParameterGroup)

-- | An array of parameter names to reset to their default values. If
-- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
-- @ResetAllParameters@ is @false@, you must specify the name of at least
-- one parameter to reset.
resetCacheParameterGroup_parameterNameValues :: Lens.Lens' ResetCacheParameterGroup (Core.Maybe [ParameterNameValue])
resetCacheParameterGroup_parameterNameValues = Lens.lens (\ResetCacheParameterGroup' {parameterNameValues} -> parameterNameValues) (\s@ResetCacheParameterGroup' {} a -> s {parameterNameValues = a} :: ResetCacheParameterGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the cache parameter group to reset.
resetCacheParameterGroup_cacheParameterGroupName :: Lens.Lens' ResetCacheParameterGroup Core.Text
resetCacheParameterGroup_cacheParameterGroupName = Lens.lens (\ResetCacheParameterGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ResetCacheParameterGroup' {} a -> s {cacheParameterGroupName = a} :: ResetCacheParameterGroup)

instance Core.AWSRequest ResetCacheParameterGroup where
  type
    AWSResponse ResetCacheParameterGroup =
      CacheParameterGroupNameMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ResetCacheParameterGroupResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable ResetCacheParameterGroup

instance Core.NFData ResetCacheParameterGroup

instance Core.ToHeaders ResetCacheParameterGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ResetCacheParameterGroup where
  toPath = Core.const "/"

instance Core.ToQuery ResetCacheParameterGroup where
  toQuery ResetCacheParameterGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ResetCacheParameterGroup" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "ResetAllParameters" Core.=: resetAllParameters,
        "ParameterNameValues"
          Core.=: Core.toQuery
            ( Core.toQueryList "ParameterNameValue"
                Core.<$> parameterNameValues
            ),
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName
      ]
