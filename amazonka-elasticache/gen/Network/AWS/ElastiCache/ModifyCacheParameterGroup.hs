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
-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a cache parameter group. You can modify up to
-- 20 parameters in a single request by submitting a list parameter name
-- and value pairs.
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
  ( -- * Creating a Request
    ModifyCacheParameterGroup (..),
    newModifyCacheParameterGroup,

    -- * Request Lenses
    modifyCacheParameterGroup_cacheParameterGroupName,
    modifyCacheParameterGroup_parameterNameValues,

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

-- | Represents the input of a @ModifyCacheParameterGroup@ operation.
--
-- /See:/ 'newModifyCacheParameterGroup' smart constructor.
data ModifyCacheParameterGroup = ModifyCacheParameterGroup'
  { -- | The name of the cache parameter group to modify.
    cacheParameterGroupName :: Prelude.Text,
    -- | An array of parameter names and values for the parameter update. You
    -- must supply at least one parameter name and value; subsequent arguments
    -- are optional. A maximum of 20 parameters may be modified per request.
    parameterNameValues :: [ParameterNameValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCacheParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupName', 'modifyCacheParameterGroup_cacheParameterGroupName' - The name of the cache parameter group to modify.
--
-- 'parameterNameValues', 'modifyCacheParameterGroup_parameterNameValues' - An array of parameter names and values for the parameter update. You
-- must supply at least one parameter name and value; subsequent arguments
-- are optional. A maximum of 20 parameters may be modified per request.
newModifyCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Prelude.Text ->
  ModifyCacheParameterGroup
newModifyCacheParameterGroup
  pCacheParameterGroupName_ =
    ModifyCacheParameterGroup'
      { cacheParameterGroupName =
          pCacheParameterGroupName_,
        parameterNameValues = Prelude.mempty
      }

-- | The name of the cache parameter group to modify.
modifyCacheParameterGroup_cacheParameterGroupName :: Lens.Lens' ModifyCacheParameterGroup Prelude.Text
modifyCacheParameterGroup_cacheParameterGroupName = Lens.lens (\ModifyCacheParameterGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ModifyCacheParameterGroup' {} a -> s {cacheParameterGroupName = a} :: ModifyCacheParameterGroup)

-- | An array of parameter names and values for the parameter update. You
-- must supply at least one parameter name and value; subsequent arguments
-- are optional. A maximum of 20 parameters may be modified per request.
modifyCacheParameterGroup_parameterNameValues :: Lens.Lens' ModifyCacheParameterGroup [ParameterNameValue]
modifyCacheParameterGroup_parameterNameValues = Lens.lens (\ModifyCacheParameterGroup' {parameterNameValues} -> parameterNameValues) (\s@ModifyCacheParameterGroup' {} a -> s {parameterNameValues = a} :: ModifyCacheParameterGroup) Prelude.. Lens._Coerce

instance Core.AWSRequest ModifyCacheParameterGroup where
  type
    AWSResponse ModifyCacheParameterGroup =
      CacheParameterGroupNameMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyCacheParameterGroupResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ModifyCacheParameterGroup

instance Prelude.NFData ModifyCacheParameterGroup

instance Core.ToHeaders ModifyCacheParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyCacheParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyCacheParameterGroup where
  toQuery ModifyCacheParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyCacheParameterGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName,
        "ParameterNameValues"
          Core.=: Core.toQueryList
            "ParameterNameValue"
            parameterNameValues
      ]
