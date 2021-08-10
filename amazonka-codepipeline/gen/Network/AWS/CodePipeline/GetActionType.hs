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
-- Module      : Network.AWS.CodePipeline.GetActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an action type created for an external
-- provider, where the action is to be used by customers of the external
-- provider. The action can be created with any supported integration
-- model.
module Network.AWS.CodePipeline.GetActionType
  ( -- * Creating a Request
    GetActionType (..),
    newGetActionType,

    -- * Request Lenses
    getActionType_category,
    getActionType_owner,
    getActionType_provider,
    getActionType_version,

    -- * Destructuring the Response
    GetActionTypeResponse (..),
    newGetActionTypeResponse,

    -- * Response Lenses
    getActionTypeResponse_actionType,
    getActionTypeResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetActionType' smart constructor.
data GetActionType = GetActionType'
  { -- | Defines what kind of action can be taken in the stage. The following are
    -- the valid values:
    --
    -- -   @Source@
    --
    -- -   @Build@
    --
    -- -   @Test@
    --
    -- -   @Deploy@
    --
    -- -   @Approval@
    --
    -- -   @Invoke@
    category :: ActionCategory,
    -- | The creator of an action type that was created with any supported
    -- integration model. There are two valid values: @AWS@ and @ThirdParty@.
    owner :: Prelude.Text,
    -- | The provider of the action type being called. The provider name is
    -- specified when the action type is created.
    provider :: Prelude.Text,
    -- | A string that describes the action type version.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'getActionType_category' - Defines what kind of action can be taken in the stage. The following are
-- the valid values:
--
-- -   @Source@
--
-- -   @Build@
--
-- -   @Test@
--
-- -   @Deploy@
--
-- -   @Approval@
--
-- -   @Invoke@
--
-- 'owner', 'getActionType_owner' - The creator of an action type that was created with any supported
-- integration model. There are two valid values: @AWS@ and @ThirdParty@.
--
-- 'provider', 'getActionType_provider' - The provider of the action type being called. The provider name is
-- specified when the action type is created.
--
-- 'version', 'getActionType_version' - A string that describes the action type version.
newGetActionType ::
  -- | 'category'
  ActionCategory ->
  -- | 'owner'
  Prelude.Text ->
  -- | 'provider'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  GetActionType
newGetActionType
  pCategory_
  pOwner_
  pProvider_
  pVersion_ =
    GetActionType'
      { category = pCategory_,
        owner = pOwner_,
        provider = pProvider_,
        version = pVersion_
      }

-- | Defines what kind of action can be taken in the stage. The following are
-- the valid values:
--
-- -   @Source@
--
-- -   @Build@
--
-- -   @Test@
--
-- -   @Deploy@
--
-- -   @Approval@
--
-- -   @Invoke@
getActionType_category :: Lens.Lens' GetActionType ActionCategory
getActionType_category = Lens.lens (\GetActionType' {category} -> category) (\s@GetActionType' {} a -> s {category = a} :: GetActionType)

-- | The creator of an action type that was created with any supported
-- integration model. There are two valid values: @AWS@ and @ThirdParty@.
getActionType_owner :: Lens.Lens' GetActionType Prelude.Text
getActionType_owner = Lens.lens (\GetActionType' {owner} -> owner) (\s@GetActionType' {} a -> s {owner = a} :: GetActionType)

-- | The provider of the action type being called. The provider name is
-- specified when the action type is created.
getActionType_provider :: Lens.Lens' GetActionType Prelude.Text
getActionType_provider = Lens.lens (\GetActionType' {provider} -> provider) (\s@GetActionType' {} a -> s {provider = a} :: GetActionType)

-- | A string that describes the action type version.
getActionType_version :: Lens.Lens' GetActionType Prelude.Text
getActionType_version = Lens.lens (\GetActionType' {version} -> version) (\s@GetActionType' {} a -> s {version = a} :: GetActionType)

instance Core.AWSRequest GetActionType where
  type
    AWSResponse GetActionType =
      GetActionTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetActionTypeResponse'
            Prelude.<$> (x Core..?> "actionType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetActionType

instance Prelude.NFData GetActionType

instance Core.ToHeaders GetActionType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.GetActionType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetActionType where
  toJSON GetActionType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("category" Core..= category),
            Prelude.Just ("owner" Core..= owner),
            Prelude.Just ("provider" Core..= provider),
            Prelude.Just ("version" Core..= version)
          ]
      )

instance Core.ToPath GetActionType where
  toPath = Prelude.const "/"

instance Core.ToQuery GetActionType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetActionTypeResponse' smart constructor.
data GetActionTypeResponse = GetActionTypeResponse'
  { -- | The action type information for the requested action type, such as the
    -- action type ID.
    actionType :: Prelude.Maybe ActionTypeDeclaration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetActionTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'getActionTypeResponse_actionType' - The action type information for the requested action type, such as the
-- action type ID.
--
-- 'httpStatus', 'getActionTypeResponse_httpStatus' - The response's http status code.
newGetActionTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetActionTypeResponse
newGetActionTypeResponse pHttpStatus_ =
  GetActionTypeResponse'
    { actionType =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The action type information for the requested action type, such as the
-- action type ID.
getActionTypeResponse_actionType :: Lens.Lens' GetActionTypeResponse (Prelude.Maybe ActionTypeDeclaration)
getActionTypeResponse_actionType = Lens.lens (\GetActionTypeResponse' {actionType} -> actionType) (\s@GetActionTypeResponse' {} a -> s {actionType = a} :: GetActionTypeResponse)

-- | The response's http status code.
getActionTypeResponse_httpStatus :: Lens.Lens' GetActionTypeResponse Prelude.Int
getActionTypeResponse_httpStatus = Lens.lens (\GetActionTypeResponse' {httpStatus} -> httpStatus) (\s@GetActionTypeResponse' {} a -> s {httpStatus = a} :: GetActionTypeResponse)

instance Prelude.NFData GetActionTypeResponse
