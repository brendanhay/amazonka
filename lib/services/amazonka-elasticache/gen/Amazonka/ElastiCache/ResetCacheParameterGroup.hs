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
-- Module      : Amazonka.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ElastiCache.ResetCacheParameterGroup
  ( -- * Creating a Request
    ResetCacheParameterGroup (..),
    newResetCacheParameterGroup,

    -- * Request Lenses
    resetCacheParameterGroup_parameterNameValues,
    resetCacheParameterGroup_resetAllParameters,
    resetCacheParameterGroup_cacheParameterGroupName,

    -- * Destructuring the Response
    CacheParameterGroupNameMessage (..),
    newCacheParameterGroupNameMessage,

    -- * Response Lenses
    cacheParameterGroupNameMessage_cacheParameterGroupName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ResetCacheParameterGroup@ operation.
--
-- /See:/ 'newResetCacheParameterGroup' smart constructor.
data ResetCacheParameterGroup = ResetCacheParameterGroup'
  { -- | An array of parameter names to reset to their default values. If
    -- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
    -- @ResetAllParameters@ is @false@, you must specify the name of at least
    -- one parameter to reset.
    parameterNameValues :: Prelude.Maybe [ParameterNameValue],
    -- | If @true@, all parameters in the cache parameter group are reset to
    -- their default values. If @false@, only the parameters listed by
    -- @ParameterNameValues@ are reset to their default values.
    --
    -- Valid values: @true@ | @false@
    resetAllParameters :: Prelude.Maybe Prelude.Bool,
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
-- 'parameterNameValues', 'resetCacheParameterGroup_parameterNameValues' - An array of parameter names to reset to their default values. If
-- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
-- @ResetAllParameters@ is @false@, you must specify the name of at least
-- one parameter to reset.
--
-- 'resetAllParameters', 'resetCacheParameterGroup_resetAllParameters' - If @true@, all parameters in the cache parameter group are reset to
-- their default values. If @false@, only the parameters listed by
-- @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
--
-- 'cacheParameterGroupName', 'resetCacheParameterGroup_cacheParameterGroupName' - The name of the cache parameter group to reset.
newResetCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Prelude.Text ->
  ResetCacheParameterGroup
newResetCacheParameterGroup pCacheParameterGroupName_ =
  ResetCacheParameterGroup'
    { parameterNameValues =
        Prelude.Nothing,
      resetAllParameters = Prelude.Nothing,
      cacheParameterGroupName =
        pCacheParameterGroupName_
    }

-- | An array of parameter names to reset to their default values. If
-- @ResetAllParameters@ is @true@, do not use @ParameterNameValues@. If
-- @ResetAllParameters@ is @false@, you must specify the name of at least
-- one parameter to reset.
resetCacheParameterGroup_parameterNameValues :: Lens.Lens' ResetCacheParameterGroup (Prelude.Maybe [ParameterNameValue])
resetCacheParameterGroup_parameterNameValues = Lens.lens (\ResetCacheParameterGroup' {parameterNameValues} -> parameterNameValues) (\s@ResetCacheParameterGroup' {} a -> s {parameterNameValues = a} :: ResetCacheParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | If @true@, all parameters in the cache parameter group are reset to
-- their default values. If @false@, only the parameters listed by
-- @ParameterNameValues@ are reset to their default values.
--
-- Valid values: @true@ | @false@
resetCacheParameterGroup_resetAllParameters :: Lens.Lens' ResetCacheParameterGroup (Prelude.Maybe Prelude.Bool)
resetCacheParameterGroup_resetAllParameters = Lens.lens (\ResetCacheParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetCacheParameterGroup' {} a -> s {resetAllParameters = a} :: ResetCacheParameterGroup)

-- | The name of the cache parameter group to reset.
resetCacheParameterGroup_cacheParameterGroupName :: Lens.Lens' ResetCacheParameterGroup Prelude.Text
resetCacheParameterGroup_cacheParameterGroupName = Lens.lens (\ResetCacheParameterGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ResetCacheParameterGroup' {} a -> s {cacheParameterGroupName = a} :: ResetCacheParameterGroup)

instance Core.AWSRequest ResetCacheParameterGroup where
  type
    AWSResponse ResetCacheParameterGroup =
      CacheParameterGroupNameMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ResetCacheParameterGroupResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ResetCacheParameterGroup where
  hashWithSalt _salt ResetCacheParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` parameterNameValues
      `Prelude.hashWithSalt` resetAllParameters
      `Prelude.hashWithSalt` cacheParameterGroupName

instance Prelude.NFData ResetCacheParameterGroup where
  rnf ResetCacheParameterGroup' {..} =
    Prelude.rnf parameterNameValues
      `Prelude.seq` Prelude.rnf resetAllParameters
      `Prelude.seq` Prelude.rnf cacheParameterGroupName

instance Data.ToHeaders ResetCacheParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetCacheParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetCacheParameterGroup where
  toQuery ResetCacheParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetCacheParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "ParameterNameValues"
          Data.=: Data.toQuery
            ( Data.toQueryList "ParameterNameValue"
                Prelude.<$> parameterNameValues
            ),
        "ResetAllParameters" Data.=: resetAllParameters,
        "CacheParameterGroupName"
          Data.=: cacheParameterGroupName
      ]
