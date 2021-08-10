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
import qualified Network.AWS.Prelude as Prelude
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
    resetAllParameters :: Prelude.Maybe Prelude.Bool,
    -- | An array of parameter names to reset to their default values. If
    -- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
    -- @ResetAllParameters@ is @false@, you must specify the name of at least
    -- one parameter to reset.
    parameterNameValues :: Prelude.Maybe [ParameterNameValue],
    -- | The name of the cache parameter group to reset.
    cacheParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ResetCacheParameterGroup
newResetCacheParameterGroup pCacheParameterGroupName_ =
  ResetCacheParameterGroup'
    { resetAllParameters =
        Prelude.Nothing,
      parameterNameValues = Prelude.Nothing,
      cacheParameterGroupName =
        pCacheParameterGroupName_
    }

-- | If @true@, all parameters in the cache parameter group are reset to
-- their default values. If @false@, only the parameters listed by
-- @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
resetCacheParameterGroup_resetAllParameters :: Lens.Lens' ResetCacheParameterGroup (Prelude.Maybe Prelude.Bool)
resetCacheParameterGroup_resetAllParameters = Lens.lens (\ResetCacheParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetCacheParameterGroup' {} a -> s {resetAllParameters = a} :: ResetCacheParameterGroup)

-- | An array of parameter names to reset to their default values. If
-- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
-- @ResetAllParameters@ is @false@, you must specify the name of at least
-- one parameter to reset.
resetCacheParameterGroup_parameterNameValues :: Lens.Lens' ResetCacheParameterGroup (Prelude.Maybe [ParameterNameValue])
resetCacheParameterGroup_parameterNameValues = Lens.lens (\ResetCacheParameterGroup' {parameterNameValues} -> parameterNameValues) (\s@ResetCacheParameterGroup' {} a -> s {parameterNameValues = a} :: ResetCacheParameterGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the cache parameter group to reset.
resetCacheParameterGroup_cacheParameterGroupName :: Lens.Lens' ResetCacheParameterGroup Prelude.Text
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

instance Prelude.Hashable ResetCacheParameterGroup

instance Prelude.NFData ResetCacheParameterGroup

instance Core.ToHeaders ResetCacheParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResetCacheParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ResetCacheParameterGroup where
  toQuery ResetCacheParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ResetCacheParameterGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ResetAllParameters" Core.=: resetAllParameters,
        "ParameterNameValues"
          Core.=: Core.toQuery
            ( Core.toQueryList "ParameterNameValue"
                Prelude.<$> parameterNameValues
            ),
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName
      ]
